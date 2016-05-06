#lang racket

;; Dependencies
(require srfi/1)
(require srfi/13)
(require srfi/48)

#|----------Racket MUD----------|#
#|                              |#
#| Author: Joao F B S Martins   |#
#|                              |#
#| Contact: joaofbdsm@gmail.com |#
#|                              |#
#|------------------------------|#

#|-------------------|#
#|  Data Structures  |#
#|-------------------|#

;; Objects description
(define objects '((1 "a steel sword")
                  (2 "an empty cup")
                  (3 "a holy cross")
                  (5 "an interdimensional communicator")))

;; Rooms description
(define descriptions '((1 "You are in the ancient ruins entrance.")
                       (2 "You are in the old great hall.")
                       (3 "You are in a chapel.")
                       (4 "You are in a graveyard.")
                       (5 "You are in a mystic room.")))

;; Initializes the object database
(define objectdb (make-hash))

;; Initializes the inventory database
(define inventorydb (make-hash))

;; Lists of pairs. First we have the user's entry and second we have what our software should understand with that entry
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define help '(((help) help)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))

;; Lists using unquote-splicing to dynamically reference all the other lists
(define actions `(,@look ,@pick ,@drop ,@inventory ,@help ,@quit))
(define decisiontable `((1 ((north) 2) ,@actions)
                        (2 ((south) 1) ((north east) 3) ((north west) 4) ,@actions)
                        (3 ((west) 4) ((south west) 2) ((north east) 5) ,@actions)
                        (4 ((south east) 2) ((east) 3) ,@actions)
                        (5 ((south west) 3) ,@actions)))

#|-------------------|#
#|     Functions     |#
#|-------------------|#

;; The usage of our previous defined function gives us a way to retrieve our location name by passing the room id. We also show the objects on the ground for the current room
(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  (printf "> "))

;; Retrieves the cdr of the first pair in assqlist where the car is equals to id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;; Displaying objects in the room and in inventory(Need to show cases when any is empty)
(define (display-objects db id)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or bag)
    (let* ((record (hash-ref db id))
            ;; Formats the output(list of items in the room)
            (output (string-join record " and ")))
      ;; Shows items in inventory or in the ground. Adds treatment to cases where the room or the inventory are empty
      (cond
        ((and (equal? output "") (eq? id 'bag)) (printf "Your inventory is empty.\n"))
        ((and (equal? output "") (number? id)) (printf "The room is empty.\n"))
        ((and (not (equal? output "")) (eq? id 'bag)) (printf "You are carrying ~a.\n" output))
        (else (printf "You see ~a.\n" output))))))

;; Receives the id(current room number) and a list of symbols that represents the user input(tokens)
(define (lookup id tokens)
  ;; Assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         ;; Assigns to keylist a list with the valid keywords for the game
         (keylist (get-keywords id))
         ;; By calling list-of-lengths, creates a list of lengths with the most probable commands and then decide which one is the most probable using index-of-largest-number
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    ;; If there is an index(prevent errors), retrieves the command that is present in that index inside the list record(list which contains the valid actions for the current room). Otherwise returns false
    (if index 
      (cadr (list-ref record index))
      #f)))

;; The same as assq-ref but uses eqv? for comparison instead of eq?
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;; Retrieve the valid keywords for the game
(define (get-keywords id)
  ;; Assigns to keys a list with the possible actions for the current room
  (let ((keys (assq-ref decisiontable id)))
    ;; Return the accepted keywords(not their actions)
    (map (lambda (key) (car key)) keys)))

;; Returns a list of lengths that shows the most probable commands given by the user. e.g. (0 0 0 3 0 0 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     ;; Returns the intersection between the tokens list(command given) and the keyword
     (let ((set (lset-intersection eq? tokens x)))
       ;; If there is an intersection between the lists, the length of set will not be zero, and thus, the result will have some weight
       (* (/ (length set) (length x)) (length set))))
   keylist))

;; Returns the most probable input command
(define (index-of-largest-number list-of-numbers)
  ;; Sorts the list of lengths in descending order and gets the first element(greatest)
  (let ((n (car (sort list-of-numbers >))))
    ;; Checks if the list is not empty(returns #f if the greatest element is 0)
    (if (zero? n)
      #f
      ;; Returns the index of the entry with the greatest weight, so it can be matched with the list of keywords later
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;; Retrieve what directions you see from the room you are
(define (get-directions id)
  ;; Describe objects that are present in the room
  (display-objects objectdb id)
  ;; In list decisiontable, finds the pair that has car equals to id and assign it to record
  (let ((record (assq id decisiontable)))
    ;; record goes through a filter and if the second value of it is a number(this is a room), it is assigned to result. Also gets the length of n(rooms you can go to)
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;; Conditional case used to finally check the directions
      (cond ((= 0 n)
             ;; 0 directions were retrieved
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             ;; Extract the directions from result using our slist->string function
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             ;; The first line(losym) in let* remove the indexes(numbers) from the directions. The second one(lostr) transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;; Takes the atoms from lostr and transform them into a string separated by " and "
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

;; Maps the parameter to a list of atoms and then joins it in a string with separator " " 
(define (slist->string l)
  (string-join (map symbol->string l)))

;; Picking up objects
(define (pick-item id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

;; Remove object from the room and add to your bag
(define (remove-object-from-room db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room)
    (let* ((record (hash-ref db id))
            ;; Remove the occurrence of the item(based on the sufix, which is the most probable user input e.g. dagger) from the room
            (result (remove (lambda (x) (string-suffix-ci? str x)) record))
            ;; Return the items that record have and result don't
            (item (lset-difference equal? record result)))
      (cond ((null? item)
             ;; If item is null(item is not in the room), reports error
             (printf "I don't see that item in the room!\n"))
            (else
              (printf "Added ~a to your bag.\n" (first item))
              ;; Adds item to inventorydb
              (add-object inventorydb 'bag (first item))
              ;; Checks if the item interacted with is the interdimensional communicator. If it is, the game is over
              (if (eq? (first item) "an interdimensional communicator")
                (begin
                  ;; Shows message and exits game
                  (printf "Something strange is happening...\nYOU HAVE FOUND THE WAY TO FREEDOM!\n")
                  (exit))
                ;; Removes item from the ground  
                (hash-set! db id result)))))))

;; Dropping objects
(define (drop-item id input)
  ;; Removes the command from the input, getting only the name of the item
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

;; Remove object from your bag and add it to the room
(define (remove-object-from-inventory db id str)
  ;; When key(id) has something stored in db, proceed
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
             (result (remove (lambda (x) (string-suffix-ci? str x)) record))
             (item (lset-difference equal? record result)))
      (cond ((null? item)
              (printf "You are not carrying that item!\n"))
             (else
              (printf "Removed ~a from your bag.\n" (first item))
              (add-object objectdb id (first item))
              (hash-set! db 'bag result))))))

;; Displays the content of your inventory
(define (display-inventory)
  (display-objects inventorydb 'bag))

;; Print the Help text on the screen
(define (display-help)
  (printf "\nHELP\n
          This is the help file for Racket MUD.\n\n 
          GAME OBJECTIVE\n
          The game objective is to activate the nether portal to escape the maze. 
          To be able to open the portal, you must find the Interdimensional Communicator
          in one of the maze rooms.\n\n
          VALID COMMANDS\n
          - (look | directions | examine room): Retrieve information about the current room.\n
          - (pick | get | pickup) <item-name> : Your character pick up the item correspondent to <item-name>. If no <item-name> is supplied, it picks up the first item on the rooms list.\n
          - (drop | put | place | remove) <item-name> : Your character throws the item correspondent to <item-name> in your bag on the ground. If no <item-name> is supplied, it drops the first item on your inventory. \n
          - (inventory | bag) : Shows a list composed by the items present in your inventory at the time.\n
          - (help) : Shows the help file for Racket MUD.\n
          - (quit | exit | quit game | exit game) : Quit the application.\n
          "))

;; Adds a given object to a database(inventory or object dbs)
(define (add-object db id object)
  ;; Returns true if id is stored in the database and false otherwise
  (if (hash-has-key? db id)
    ;; Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or bag)
    (let ((record (hash-ref db id)))
      ;; Assigns to the table key(id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons object record)))
    ;; Assigns the object(consed with '() to become a list) to a key(id) in the hash table
    (hash-set! db id (cons object empty))))

;; Pregame population of rooms with objects
(define (add-objects db)
  (for-each
    (lambda (r)
      ;; Adds description(second r) to room id(first r)
      (add-object db (first r) (second r))) objects))

#|-------------------|#
#|     Game Loop     |#
#|-------------------|#

;; This is the main game loop. The id refers to the room in which the player is.
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        ;; If there is an available description, shows it on the screen
        (get-location id)
        ;; Else statement. Don't show location(because there isn't any description). Just shows the greater than symbol to incite user to type in text field
        (printf "> "))
    ;; Read input from the keyboard
    (let* ((input (read-line))
           ;; Function contained in the srfi/13 library, tokenize the input into substrings where a space character is found
           (string-tokens (string-tokenize input))
           ;; Creates a list of symbols(not strings) with the input. This is needed to compare the entry with our predefined lists
           (tokens (map string->symbol string-tokens)))
      ;; Decides which action response corresponds to. One of the most important calls in the code 
      (let ((response (lookup id tokens)))
        ;; (printf "Input: ~a\nTokens: ~a\nResponse: ~a\n" input tokens response)
        (cond ((number? response)
               (loop response #t))
              ;; If response meaning couldn't be found after the lookup function, shows error message
              ((eq? #f response)
               (format #t "Huh? I didn't understand that!\n")
               (loop id #f))
              ;; Response action is look at around the room for directions
              ((eq? response 'look)
               ;; Retrieve possible directions
               (get-directions id)
               (loop id #f))
              ;; Response action is to pick an item
              ((eq? response 'pick)
               ;; Pick up item
               (pick-item id input)
               (loop id #f))
              ;; Response action is to drop an item
              ((eq? response 'drop)
               ;; Drop item
               (drop-item id input)
               (loop id #f))
              ;; Response action is to show inventory
              ((eq? response 'inventory)
               ;; Displays the inventory
               (display-inventory)
               (loop id #f))
              ;; Response action is to display the help file
              ((eq? response 'help)
                ;; Displays Help text on the screen
                (display-help)
                (loop id #f))
              ;; Exit game command
              ((eq? response 'quit)
               ;; Exit the application
               (format #t "Hasta la vista, baby!\n")
               (exit)))))))

;; Adds the objects to the database before the game starts
(add-objects objectdb)

;; Start game in the first room
(startgame 1)