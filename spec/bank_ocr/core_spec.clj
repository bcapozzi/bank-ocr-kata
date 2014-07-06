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

          (it "can determine valid account numbers"
              (should= true (is-valid? "711111111"))
              (should= true (is-valid? "123456789"))
              (should= true (is-valid? "490867715"))
              )
          
          (it "can determine invalid account numbers"
              (should= false (is-valid? "888888888"))
              (should= false (is-valid? "490067715"))
              (should= false (is-valid? "012345678"))
              )

          ;; User Story 3 - create log entry for each account number
          (it "can create a log entry for a valid account number"
              (should= "123456789" (create-log-entry-for "123456789"))
              )

          (it "can create a log entry for an illegible account number"
              (should= "123?56789 ILL" (create-log-entry-for "123?56789"))
              )

          (it "can create a log entry for an invalid account number"
              (should= "111111111 ERR" (create-log-entry-for "111111111"))
              )
          )
