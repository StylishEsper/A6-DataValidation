       identification division.
       program-id. Program1.
      *Program Description: This program reads from a .dat file and
      *outputs a report indicating invalid or valid data
       environment division.

       input-output section.
       file-control.
      *input-file: file to read from
           select input-file
               assign to "../../../A6.dat"
               organization is line sequential.
      *output-file: file to write into
           select output-file
               assign to 
               "../../../A6-DataValidation.out"
               organization is line sequential.

       configuration section.

       data division.
       file section.

       fd input-file
           data record is input-line
           record contains 24 characters.
      *input-line: stores data from .dat file
       01 input-line.
           05 il-maintenance-code  pic x.
           05 il-part-number       pic 999.
           05 il-part-description  pic x(10).
           05 il-per-unit-price    pic 99v99.
           05 il-vendor-number.
               10 il-vendor-first  pic 9.
                   88 il-vendor-check
                       value 1, 2, 3.
               10 il-vendor-others pic x(5).

      *output-line: used to write into report file
       fd output-file
           data record is output-line
           record contains 40 characters.

       01 output-line pic x(40).

       working-storage section.

      *ws-detail-line: will display the record number and raw data
       01 ws-detail-line.
           05 ws-record-number     pic 99.
           05 filler               pic x(14) value spaces.
           05 ws-data              pic x(24).

      *ws-error-line: stores the error/valid message
       01 ws-error-line.
           05 filler               pic x(16) value spaces.
           05 ws-error             pic x(24).

      *the following are constants or headings 
       77 ws-true-cnst             pic x value "T".
       77 ws-false-cnst            pic x value "F".
       77 ws-lines-per-page-cnst   pic 99 value 14.
      *
       77 ws-heading1              pic x(24) value 
       "Ahmed Butt, Assignment 6".
      *
       01 ws-heading2.
           05 title-error          pic x(38) value 
       "                ERROR REPORT      Page".
           05 filler               pic x value spaces.
           05 page-num             pic 9.
      *
       77 ws-heading3              pic x(20) value 
       "Record Number   Data".
      *
       77 ws-heading4              pic x(40) value
       "----------------------------------------".
      *
       01 ws-line-page-counters.
           05 ws-line-count        pic 99 value 0.
           05 ws-page-count        pic 99 value 0.
      *
       01 ws-eof-flag              pic x.
      *
       01 ws-record-count          pic 99 value 0.

      *the following display variables are meant to display the totals
      *found in the last page of the report
       01 ws-display-records.
           05 filler               pic x(14) value "INPUT        -".
           05 filler               pic x value spaces.
           05 ws-records           pic z9.
      *
       01 ws-display-valid.
           05 filler               pic x(14) value "GOOD         -".
           05 filler               pic x value spaces.
           05 ws-valid             pic z9.
      *
       01 ws-display-invalid.
           05 filler               pic x(14) value "IN ERROR     -".
           05 filler               pic x value spaces.
           05 ws-invalid           pic z9.
      *
       01 ws-display-validA.
           05 filler               pic x(14) value "GOOD ADDS    -".
           05 filler               pic x value spaces.
           05 ws-validA            pic z9.
      *
       01 ws-display-validC.
           05 filler               pic x(14) value "GOOD CHANGES -".
           05 filler               pic x value spaces.
           05 ws-validC            pic z9.
      *
       01 ws-display-validD.
           05 filler               pic x(14) value "GOOD DELETES -".
           05 filler               pic x value spaces.
           05 ws-validD            pic z9.

      *ws-calc: storing totals
       01 ws-calc.
           05 ws-count-records     pic 99 value 0.
           05 ws-count-valid       pic 99 value 0.
           05 ws-count-invalid     pic 99 value 0.
           05 ws-count-validA      pic 99 value 0.
           05 ws-count-validC      pic 99 value 0.
           05 ws-count-validD      pic 99 value 0.


       procedure division.
       000-main.
           open input input-file.
           open output output-file.

           read input-file
               at end
                   move ws-true-cnst to ws-eof-flag.

           perform 100-process-pages
             until ws-eof-flag = ws-true-cnst.

      *moving totals to display variables
           move ws-count-records to ws-records.
           move ws-count-valid to ws-valid.
           move ws-count-invalid to ws-invalid.
           move ws-count-validA to ws-validA.
           move ws-count-validC to ws-validC.
           move ws-count-validD to ws-validD.

      *writing final page totals to the report
           write output-line from ws-heading4.
           write output-line from ws-display-records.
           write output-line from " ".
           write output-line from ws-display-valid.
           write output-line from " ".
           write output-line from ws-display-invalid.
           write output-line from " ".
           write output-line from ws-display-validA.
           write output-line from " ".
           write output-line from ws-display-validC.
           write output-line from " ".
           write output-line from ws-display-validD.

           close input-file, output-file.

           goback.

      *prints headings and begins a loop for lines
       100-process-pages.

           move 0 to ws-line-count.

           perform 200-print-headings

           perform 300-process-lines
             until ws-eof-flag = ws-true-cnst
             OR ws-line-count > ws-lines-per-page-cnst.

      *prints appropriate headings depending on the page number
       200-print-headings.
      *
           add 1 to ws-page-count.
           move ws-page-count to page-num.

           if ws-page-count = 1
               write output-line from ws-heading1
           else
               write output-line from " " after advancing page
           end-if.
      *
           write output-line from " ".
           write output-line from ws-heading2.
           write output-line from " ".
           write output-line from ws-heading3.
           write output-line from ws-heading4.

      *displays details lines, error lines, adds to total counters,
      *determined through a series of validation (if statements)
       300-process-lines.
      *
           add 1 to ws-record-count.
           add 1 to ws-line-count.
           move ws-record-count to ws-record-number.
           move input-line to ws-data.

           write output-line from ws-detail-line.

           move "VALID DATA" to ws-error.

           add 1 to ws-count-records.
      *    
           if il-maintenance-code <> "A" and "C" and "D"
             then
               move "WRONG MAINT CODE" to ws-error
               write output-line from ws-error-line
           end-if.
      *
           if il-maintenance-code <> "D"
               if il-part-number is not numeric
                 then
                   move "PART NO. NOT NUMERIC" to ws-error
                   write output-line from ws-error-line
               end-if

               if il-per-unit-price < 1 or il-per-unit-price > 50 or 
               il-per-unit-price is not numeric
                 then
                   move "PRICE IN TROUBLE" to ws-error
                   write output-line from ws-error-line
               end-if

               if il-part-description = " "
                 then
                   move "DESCRIPTION MISSING" to ws-error
                   write output-line from ws-error-line
               end-if

               if il-part-description is alphabetic
                 then
                   move "NON ALPHA IN DESC" to ws-error
                   write output-line from ws-error-line
               end-if

               if not il-vendor-check
                  then
                   move "WRONG VENDOR SERIES" to ws-error
                   write output-line from ws-error-line
               end-if
           end-if.
      *
           if ws-error = "VALID DATA"
             then
               add 1 to ws-count-valid

               if il-maintenance-code = "A"
                 then
                   add 1 to ws-count-validA
               end-if

               if il-maintenance-code = "C"
                 then
                   add 1 to ws-count-validC
               end-if

               if il-maintenance-code = "D"
                 then
                   add 1 to ws-count-validD
               end-if

               write output-line from ws-error-line
           else
               add 1 to ws-count-invalid
           end-if.
      *
           write output-line from " ".

           read input-file
               at end
                   move ws-true-cnst to ws-eof-flag.

       end program Program1.