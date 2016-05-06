#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

;Objects description
(define objects '((1 "a silver dagger")
                  (2 "a gold coin")))

(define descriptions '((1 "You are in the lobby")
                       (2 "You are in the hallway")
                       (3 "You are in a swamp")))

;(listof pair?) #t
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))

;unquote-splicing(references all the other lists)
(define actions `(,@look ,@quit ,@pick ,@put ,@inventory))
(define decisiontable `((1 ((north) 2) ((north west) 3) ,@actions)
                        (2 ((south) 1) ,@actions)
                        (3 ,@actions)))

;Maps the parameter to a list of atoms and then joins it in a string with separator " " 
(define (slist->string l)
  (string-join (map symbol->string l)))

;Retrieve that directions you see from the room you are
(define (get-directions id)
  ;In list decisiontable, finds the pair that has car equals to id and assign it to record
  (let ((record (assq id decisiontable)))
    (printf "Record: ~a\n" record)
    (printf "Filter record: ~a\n" (filter (lambda (n) (number? (second n))) (cdr record)))
    ;record goes through a filter and if the second value of it is a number(this is a room), it is assigned to result. Also gets the length of n(rooms you can go to)
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      ;Conditional case used to finally check the directions
      (cond ((= 0 n)
             ;0 directions were retrieved
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             ;Extract the directions from result using our slist->string function
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             ;The first line(losym) in let* remove the indexes(numbers) from the directions. The second one(lostr) transforms the list in a lat with the directions.
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               ;Takes the atoms from lostr and transform them into a string separated by " and "
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

;Retrieves the cdr of the first pair in assqlist where the car is equals to id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

;The same as assq-ref but uses eqv? for comparison instead of eq?
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;The usage of our previous defined function gives us a way to retrieve our location name by passing the room id
(define (get-location id)
  (printf "~a\n" (car (assq-ref descriptions id)))
  (display-objects objectdb id)
  (printf "> "))

;The same that happens in get-location. Retrieves the object name based on an id
(define (get-object id)
  (car (assq-ref objects id)))

;Retrieve the valid keywords for the game
(define (get-keywords id)
  ;Assigns to keys a list with the possible actions for the current room
  (let ((keys (assq-ref decisiontable id)))
    ;Return the accepted keywords(not their actions)
    (map (lambda (key) (car key)) keys)))

;Returns a list of lengths that shows the most probable commands given by the user. e.g. (0 0 0 3 0 0 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     ;Returns the intersection between the tokens list(command given) and the keyword
     (let ((set (lset-intersection eq? tokens x)))
       ;If there is an intersection between the lists, the length of set will not be zero, and thus, the result will have some weight
       (* (/ (length set) (length x)) (length set))))
   keylist))

;Returns the most probable input command
(define (index-of-largest-number list-of-numbers)
  ;Sorts the list of lengths in descending order and gets the first element(greatest)
  (let ((n (car (sort list-of-numbers >))))
    ;Checks if the list is not empty(returns #f if the greatest element is 0)
    (if (zero? n)
      #f
      ;Returns the index of the entry with the greatest weight, so it can be matched with the list of keywords later
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;Receives the id(current room number) and a list of symbols that represents the user input(tokens)
(define (lookup id tokens)
  ;Assigns to record a list with the possible actions for the current room
  (let* ((record (assv-ref decisiontable id))
         ;Assigns to keylist a list with the valid keywords for the game
         (keylist (get-keywords id))
         ;By calling list-of-lengths, creates a list of lengths with the most probable commands and then decide which one is the most probable using index-of-largest-number
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    ;If there is an index(prevent errors), retrieves the command that is present in that index inside the list record(list which contains the valid actions for the current room). Otherwise returns false
    (if index 
      (cadr (list-ref record index))
      #f)))

;Initializes the object database
(define objectdb (make-hash))

;Initializes the inventory database
(define inventorydb (make-hash))

;Adds a given object to a database(inventory or object dbs)
(define (add-object db id object)
  ;Returns true if id is stored in the database and false otherwise
  (if (hash-has-key? db id)
    ;Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or bag)
    (let ((record (hash-ref db id)))
      ;Assigns to the table key(id) the cons between the actual object and the preexisting objects in the key
      (hash-set! db id (cons object record)))
    ;Assigns the object(consed with '() to become a list) to a key(id) in the hash table
    (hash-set! db id (cons object empty))))

;Pregame population of rooms with objects
(define (add-objects db)
  (for-each
    (lambda (r)
      ;Adds description(second r) to room id(first r)
      (add-object db (first r) (second r))) objects))

;Displaying objects in the room and in inventory(Need to show cases when any is empty)
(define (display-objects db id)
  ;When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room or bag)
    (let* ((record (hash-ref db id))
            ;Formats the output(list of items in the room)
            (output (string-join record " and ")))
      ;When output is not an empty String
      (when (not (equal? output ""))
        ;If user requested to see the inventory
        ;TODO: Needs some rethinking
        (if (eq? id 'bag)
          (printf "You are carrying ~a.\n" output)
          (printf "You can see ~a.\n" output ))))))

;Picking up objects
(define (remove-object-from-room db id str)
  ;When key(id) has something stored in db, proceed
  (when (hash-has-key? db id)
    ;Assigns to record the content of the key id inside the db hash table(gets previous items assigned to a room)
    (let* ((record (hash-ref db id))
            (result (remove (lambda (x) (string-suffix-ci? str x)) record))
            (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I don’t see that item in the room!\n"))
            (else
              (printf "Added ~a to your bag.\n" (first item))
              ;TODO: Improve function so it is not the first item always
              (add-object inventorydb 'bag (first item))
              (hash-set! db id result))))))

;Dropping objects
(define (remove-object-from-inventory db id str)
  ;When key(id) has something stored in db, proceed
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

(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

;This is the main game loop. The id refers to the room in which the player is.
(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        ;If there is an available description, shows it on the screen
        (get-location id)
        ;Else statement. Don't show location(because there isn't any description). Just shows the greater than symbol to incite user to type in text field
        (printf "> "))
    ;Read input from the keyboard
    (let* ((input (read-line))
           ;Function contained in the srfi/13 library, tokenize the input into substrings where a space character is found
           (string-tokens (string-tokenize input))
           ;Creates a list of symbols(not strings) with the input. This is needed to compare the entry with our predefined lists
           (tokens (map string->symbol string-tokens)))
      ; 
      (let ((response (lookup id tokens)))
        (printf "Tokens: ~a\n" tokens)
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (printf "Look ID: ~a\n" id)
               (get-directions id)
               (loop id #f))
              ((eq? response 'pick)
               (pick-item id (get-object id))
               (loop id #f))
              ((eq? response 'put)
               (put-item id (get-object id))
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

;Adds the objects to the database
(add-objects objectdb)

;Start game in the first room
(startgame 1)

;Problem put
;Show ground objects
;Items can be seen from different rooms
; Input error handling
; Fix function calls (Not receiving string) OK
; Add dungeon door
; Not seeing items on ground OK