(ns bank-ocr.core-spec
  (:require [speclj.core :refer :all]
            [bank-ocr.core :refer :all]))


(describe "scan converter"
          
          (it "can convert scan of all ZEROS to an account number"
              (let [scan-lines '(
                                 " _  _  _  _  _  _  _  _  _ \n",
                                 "| || || || || || || || || |\n",
                                 "|_||_||_||_||_||_||_||_||_|\n",
                                 "                           ")]
                (should= "000000000" (convert scan-lines 3))
                ))

          
          (it "can convert scan of all ONES to an account number"
              (let [scan-lines '(
                                 "                           \n",
                                 "  |  |  |  |  |  |  |  |  |\n",
                                 "  |  |  |  |  |  |  |  |  |\n",                                 
                                 "                           ")]
                (should= "111111111" (convert scan-lines 3))
                ))

          (it "can convert scan of all TWOS to an account number"
              (let [scan-lines '(
                                 " _  _  _  _  _  _  _  _  _ \n",
                                 " _| _| _| _| _| _| _| _| _|\n",
                                 "|_ |_ |_ |_ |_ |_ |_ |_ |_ \n",                                 
                                 "                           ")]
                (should= "222222222" (convert scan-lines 3))
                ))


          (it "can convert scan of all THREES to an account number"
              (let [scan-lines '(
                                 " _  _  _  _  _  _  _  _  _ \n",
                                 " _| _| _| _| _| _| _| _| _|\n",
                                 " _| _| _| _| _| _| _| _| _|\n",
                                 "                           ")]
                (should= "333333333" (convert scan-lines 3))
                ))

          (it "can convert scan of all FOURS to an account number"
              (let [scan-lines '(
                                  "                           \n",
                                  "|_||_||_||_||_||_||_||_||_|\n",
                                  "  |  |  |  |  |  |  |  |  |\n",
                                  "                           ")]
                (should= "444444444" (convert scan-lines 3))
                ))

          (it "can convert scan of all ONE thru NINE to an account number"
              (let [scan-lines '(
                                 "    _  _     _  _  _  _  _ \n",
                                 "  | _| _||_||_ |_   ||_||_|\n",
                                 "  ||_  _|  | _||_|  ||_| _|\n",
                                 "                           ")]
                (should= "123456789" (convert scan-lines 3))
                ))


          )
