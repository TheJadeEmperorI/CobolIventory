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
                      
                   WHEN 4 PERFORM ADD-PRODUCT
       
                   WHEN 5 PERFORM EXIT-INVENTORY
       
                   WHEN OTHER DISPLAY 'Invalid option.'
           END-PERFORM.
       
       VIEW-INVENTORY.
           DISPLAY "============================================".
           DISPLAY "              --- INVENTORY ---             ".
           DISPLAY "============================================".
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

     