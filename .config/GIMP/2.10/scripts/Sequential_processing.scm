; sequential_processing.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20120802)

; Description:
; Written to answer this request:
; There a directory with few tenth of a *.xcf images, and the operator opens them one by one starting 
; from the first image (and doing some stuff on them between the closing and opening the images). 
; The sequence I want to automate is:
; 1. save current image
; 2. close it
; 3. open next image in directory
; The question: Is there a way to automate it? If this sequence cannot be done by script, is there other ways to do it?
;
; Changes:
; v1.0 Initial release
; v1.1 Name changed, added another script to select the start file rather than just a folder
; v1.2 Option to specify file types.

; License:
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

; global defines for all scripts
(define seq-file-list-ext (list "xcf" "jpg" "bmp" "png" "tiff" "orf"))
(define seq-file-list-glob (list "[xX][cC][fF]" "[jJ][pP][gG]" "[bB][mM][pP]" "[pP][nN][gG]" "[tT][iI][fF][fF]" "[oO][rR][fF]"))

(define (seq-open-first-file inDir inType)
  (let* 
    ((fileexts (list-ref seq-file-list-glob inType))
     (fileext (list-ref seq-file-list-ext inType))      
     (filelist (cadr (file-glob (string-append inDir DIR-SEPARATOR "*." fileexts) 1)))
     (img -1)
     (disp -1)
     (parasite())
    )
    (if (= (length filelist) 0)
      (gimp-message (string-append "No " fileext " files in this directory!"))
      (begin
        (set! img (car (gimp-file-load RUN-NONINTERACTIVE (car filelist) (car filelist))))       
        (set! disp (number->string (car (gimp-display-new img))))
        (set! parasite (list "display-number" 1 disp))
        ;set parasite in image so the display can be closed later
        (gimp-image-parasite-attach img parasite)
        ;global parasite for sequential file type
        (set! parasite (list "seq-file-type" 1 (number->string inType)))
        (gimp-parasite-attach parasite)
      )
    )
    (gimp-progress-end)
  )
)

(script-fu-register "seq-open-first-file"
			"<Image>/File/Sequential/Open First File..."
			"Open the first file in a directory setting up for repeated file processing"
			"Rob Antonishen"
			"Rob Antonishen"
			"Aug 2012"
			""
            
			SF-DIRNAME    "Load from" ""
            SF-OPTION     "File Types" seq-file-list-ext
)

(define (seq-open-specific-file inFile)
  (let* 
    ((fileType (let ((i 0) (k #f)) 
                 (for-each (lambda (n) 
                   (if (string-ci=? n (car (last (strbreakup inFile ".")))) (set! k i)) 
                   (set! i (+ i 1))) seq-file-list-ext) k))
     (fileexts (if fileType (list-ref seq-file-list-glob fileType)))
     (fileext (if fileType (list-ref seq-file-list-ext fileType)))
     (img -1)
     (disp -1)
     (parasite())
    )
    (if (not fileType)
      (gimp-message (string-append "File must be one of: " (unbreakupstr seq-file-list-ext " ")))
      (begin
        (set! img (car (gimp-file-load RUN-NONINTERACTIVE inFile inFile)))
        
        (set! disp (number->string (car (gimp-display-new img))))
        (set! parasite (list "display-number" 1 disp))
        ;set parasite in image so the display can be closed later
        (gimp-image-parasite-attach img parasite)
        ;global parasite for sequential file type
        (set! parasite (list "seq-file-type" 1 (number->string fileType)))
        (gimp-parasite-attach parasite)
        (gimp-message (string-append "Sequential filetype set to " fileext))
      )
    )
    (gimp-progress-end)
  )
)

(script-fu-register "seq-open-specific-file"
			"<Image>/File/Sequential/Open Specific File..."
			"Opens a specific file in a directory setting up for repeated file processing of that filetype"
			"Rob Antonishen"
			"Rob Antonishen"
			"Aug 2012"
			""
			SF-FILENAME   "Starting File" ""
)


(define (seq-open-next-file inImg inLayer)
  (let* 
    ((filename (car (gimp-image-get-filename inImg)))
     (filelist ())
     (img -1)
     (disp -1)
     (parasite (catch #f (car (gimp-parasite-find "seq-file-type"))))
     (fileexts "")
     (fileext "")
     (fileType -1)
    )
    (if (equal? filename "") ; check that file is saved already
      (gimp-message "Not a saved file!")
      (when parasite
        ; get filetype from parasite     
        (set! fileType (string->number (caddr parasite)))       
        (set! fileexts (list-ref seq-file-list-glob fileType))
        (set! fileext (list-ref seq-file-list-ext fileType))

        ; check that the file matches the parasite file type
        (if (not (string-ci=? fileext (car (last (strbreakup (car (last (strbreakup filename DIR-SEPARATOR))) ".")))))
          (gimp-message (string-append "Not a " fileext " file!"))
          (begin
            ; save current file
            (gimp-file-save RUN-NONINTERACTIVE inImg inLayer filename (car (last (strbreakup filename DIR-SEPARATOR))))
            
            ; check for a display parasite and delete the display, erase the parasite and delete the image
            (if (catch #f (gimp-image-parasite-find inImg "display-number"))
              (gimp-display-delete (string->number (caddr (car (gimp-image-parasite-find inImg "display-number")))))
            )
  
            ; get file list starting with this one
            (set! filelist (member filename (cadr (file-glob (string-append (unbreakupstr (butlast (strbreakup filename DIR-SEPARATOR)) DIR-SEPARATOR) DIR-SEPARATOR "*." fileexts) 1))))
            ; check for more than one file in the list
            (if (= (length filelist) 1)
              (begin
                (gimp-message (string-append "Last " fileext " file in directory!"))
                ;delete filetype parasite
                (gimp-parasite-detach "seq-file-type")
              )
              (begin
                ; load up new file, display.
                (set! img (car (gimp-file-load RUN-NONINTERACTIVE (cadr filelist) (cadr filelist))))
                (set! disp (number->string (car (gimp-display-new img))))
                (set! parasite (list "display-number" 1 disp))
                ;set parasite in image so the display can be closed later
                (gimp-image-parasite-attach img parasite)
              )
            )            
          )
        )
      )
    )
    (gimp-progress-end)    
  )
)

(script-fu-register "seq-open-next-file"
			"<Image>/File/Sequential/Save and Close then Open Next File"
			"Save and close the current file then open the next file of the matching type in that directory"
			"Rob Antonishen"
			"Rob Antonishen"
			"Aug 2012"
			"*"
            SF-IMAGE      "image"      0
            SF-DRAWABLE   "drawable"   0
)