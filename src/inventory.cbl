       IDENTIFICATION DIVISION.
       PROGRAM-ID. inventory.
       AUTHOR. The Jade Emperor.
       DATE-WRITTEN. 7 March 2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO 
           "/home/lubu/cobol/CobolIventory/data/inventory.dat" 
           ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-FILE.
       01 PRODUCT_RECORD.
           05 PRODUCT-ID PIC 9(5).
           05 PRODUCT-NAME PIC x(15).
           05 PRODUCT-QUANTITY PIC 9(4).
           05 PRODUCT-PRICE PIC 9(5)V99.
       
       WORKING-STORAGE SECTION.
       01 EOF-FLAG PIC x(1).
       01 USER-CHOICE PIC 9(1).
       01 SEARCH-ID-PRODUCT PIC 9(5).
       01 FOUND PIC 9(1).

       01 NEW-QUANTITY PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM SELECT-MENU.
           STOP RUN.

       SELECT-MENU.
           PERFORM until USER-CHOICE = 5
               DISPLAY "============================================"
               DISPLAY "            --- SELECT MENU ---             "
               DISPLAY "============================================"
          
               DISPLAY '1. View Inventory'
               DISPLAY '2. Update Stock'
               DISPLAY '3. Search Product'
               DISPLAY '4. Add Product'
               DISPLAY '5. Exit'
               DISPLAY 'Enter your choice : ' WITH NO ADVANCING
               ACCEPT USER-CHOICE
       
               EVALUATE USER-CHOICE
                   WHEN 1 PERFORM VIEW-INVENTORY

                   WHEN 2 PERFORM UPDATE-STOCK

                   WHEN 3 PERFORM SEARCH-PRODUCT
                      
                   WHEN 4 PERFORM ADD-PRODUCT
       
                   WHEN 5 PERFORM EXIT-INVENTORY
       
                   WHEN OTHER DISPLAY 'Invalid option.'
           END-PERFORM.
       
       VIEW-INVENTORY.
           DISPLAY "============================================".
           DISPLAY "              --- INVENTORY ---             ".
           DISPLAY "============================================".
      *    PRODUCT-ID | PRODUCT-NAME | QUANTITY | PRICE
           OPEN INPUT INVENTORY-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ INVENTORY-FILE INTO PRODUCT_RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
               IF EOF-FLAG NOT = 'Y'
                   DISPLAY PRODUCT-ID SPACE PRODUCT-NAME SPACE 
                   PRODUCT-QUANTITY SPACE PRODUCT-PRICE
               END-IF
           END-PERFORM.
           
           MOVE 'N' TO EOF-FLAG.
           CLOSE INVENTORY-FILE.
        

       UPDATE-STOCK.
           DISPLAY "============================================".
           DISPLAY "            --- UPDATE STOCK ---            ".
           DISPLAY "============================================".

           DISPLAY "Please enter the ID PRODUCT : " WITH NO ADVANCING.
           ACCEPT SEARCH-ID-PRODUCT.
           DISPLAY "Enter the new stock : " WITH NO ADVANCING.
           ACCEPT NEW-QUANTITY.
           
           MOVE 0 TO FOUND.

           OPEN INPUT INVENTORY-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y' OR FOUND = 1
               READ INVENTORY-FILE INTO PRODUCT_RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ
               
               IF SEARCH-ID-PRODUCT = PRODUCT-ID
                   MOVE NEW-QUANTITY TO PRODUCT-QUANTITY
                   MOVE 1 TO FOUND
                   DISPLAY "Stock has been updated"
               END-IF
           
           END-PERFORM.

           IF FOUND = 0
               DISPLAY "The ID product is invalid."
           END-IF.

           CLOSE INVENTORY-FILE.
       

       SEARCH-PRODUCT.
           DISPLAY "============================================".
           DISPLAY "            --- SEARCH PRODUCT ---          ".
           DISPLAY "============================================".

           DISPLAY "Please enter the ID PRODUCT : " WITH NO ADVANCING.
           ACCEPT SEARCH-ID-PRODUCT.

           MOVE 0 TO FOUND. 

           OPEN INPUT INVENTORY-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y' OR FOUND = 1
               READ INVENTORY-FILE INTO PRODUCT_RECORD
                   AT END MOVE 'Y' TO EOF-FLAG
               END-READ

               IF SEARCH-ID-PRODUCT = PRODUCT-ID
                   DISPLAY "Found :"
                   DISPLAY PRODUCT-ID SPACE PRODUCT-NAME SPACE 
                   PRODUCT-QUANTITY SPACE PRODUCT-PRICE
                   MOVE 1 TO FOUND

               END-IF
           
           END-PERFORM.
           
           IF FOUND = 0
               DISPLAY "The ID product is invalid."
           END-IF.

           CLOSE INVENTORY-FILE.

       ADD-PRODUCT.
           DISPLAY "============================================".
           DISPLAY "            --- ADD PRODUCT ---             ".
           DISPLAY "============================================".

           DISPLAY '---Adding product---'.
           DISPLAY 'Product ID : ' WITH NO ADVANCING.
           ACCEPT PRODUCT-ID.

           DISPLAY 'Product name : ' WITH NO ADVANCING.
           ACCEPT PRODUCT-NAME.

           DISPLAY 'Product quantity : ' WITH NO ADVANCING.
           ACCEPT PRODUCT-QUANTITY.

           DISPLAY 'Product price : ' WITH NO ADVANCING.
           ACCEPT PRODUCT-PRICE.
               
           PERFORM WRITE-PRODUCT.

       WRITE-PRODUCT.
           OPEN EXTEND INVENTORY-FILE.
           WRITE PRODUCT_RECORD.

           CLOSE INVENTORY-FILE.


       EXIT-INVENTORY.
           DISPLAY 'Closing the inventory...'.
           EXIT.

     