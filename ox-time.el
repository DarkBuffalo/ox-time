;;; ox-time.el --- Export your org file to timesheet PDF file -*- lexical-binding: t -*-

;; Copyright (C) 2020  Matthias David
;; Author: Matthias David <matthias@gnu.re>
;; URL: https://github.com/DarkBuffalo/ox-time
;; Version: 0.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: org, outlines, exporter, meeting, minutes

;;; Commentary:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This is a another exporter for org-mode that translates Org-mode file to
;; beautiful PDF file
;;
;; EXAMPLE ORG FILE HEADER:
;;
;;   #+title:Readme ox-time
;;   #+author: Matthias David
;;   #+options: toc:nil
;;   #+ou:Zoom
;;   #+quand: 20/2/2021
;;   #+projet: ox-time
;;   #+absent: C. Robert,T. tartanpion
;;   #+present: K. Soulet,I. Payet
;;   #+excuse:Sophie Fonsec,Karine Soulet
;;   #+logo: logo.png
;;
;;; Code:

(require 'ox)
(require 'cl-lib)

(setq org-latex-packages-alist
      '(("" "booktabs" t)
        ("" "siunitx" t)
        ("" "longtable" t)
        ))

(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex")))

(add-to-list 'org-latex-classes
             '("time"                          ;class-name
               "\\documentclass[a4paper]{article}
\\usepackage[top=1.5cm, bottom=1.5cm, left=2cm, right=2cm]{geometry}
\\usepackage{ifxetex}
\\usepackage{ifluatex}
\\newif\\ifmodernTeX
\\modernTeXfalse
\\ifluatex
  \\modernTeXtrue
\\fi
\\ifxetex
  \\modernTeXtrue
\\fi

\\ifmodernTeX
\\usepackage{fontspec}
\\defaultfontfeatures{Ligatures=TeX}

\\setmainfont[
      BoldFont=LinLibertineOZ,
      BoldItalicFont=LinLibertineOZI,
      SmallCapsFont=LinLibertineO,
      SmallCapsFeatures={Letters=SmallCaps},
]{LinuxLibertineO}

\\newfontfamily\\spacedFont[LetterSpace=2.0]{LinuxLibertineO}

\\newfontfamily\\displayFont[Extension=.otf,
      BoldFont=LinLibertine_DR,%%fake
      ItalicFont=LinLibertine_DR,%%fake
      BoldItalicFont=LinLibertine_DR,%%fake
      ]{LinLibertine_DR}

\\setsansfont[Extension=.otf,
      BoldFont=LinBiolinum_RB,
      ItalicFont=LinBiolinum_RI,
      BoldItalicFont=LinBiolinum_RB,%% fake
      SmallCapsFont=LinBiolinum_R,
      SmallCapsFeatures={Letters=SmallCaps},
      ]{LinBiolinum_R}

\\setmonofont[Extension=.otf]{Inconsolata}

\\else

\\let\\spacedFont\\relax
\\let\\displayFont\\relax

\\fi

\\usepackage[table]{xcolor}
\\usepackage{array}
\\usepackage{multicol}
\\usepackage[norule]{footmisc}
\\RequirePackage{graphicx}

%%%%%%%%%%%%%%%%%%%%%%%
%% color definitions %%
%%%%%%%%%%%%%%%%%%%%%%%

\\colorlet{headcolor}{gray!21}
\\colorlet{tablecolor1}{gray!4}
\\colorlet{tablecolor2}{gray!11}
\\colorlet{footnotegray}{gray!90}

%% the right shift of the right blocks
\\xdef\\rightalignment{11cm}

%% footnote style
\\def\\footnotestyle#1{%%
  {\\textsf{\\color{footnotegray}\\fontsize{3mm}{0mm}\\selectfont #1}}%%
}

%% change color of footnote marks
\\makeatletter
\\renewcommand\\@makefntext[1]{%%
  \\parindent 1em\\noindent
  \\hb@xt@1.8em{%%
  \\hss\\@textsuperscript{\\normalfont\\color{footnotegray}\\@thefnmark}}#1}


%%	TABLE FORMATTING
%%----------------------------------------------------------------------------------------
\\newcommand{\\tableHeaderStyle}{
    \\rowfont{\\leavevmode\\color{black}\\bfseries}
    \\rowcolor{headcolor}
}

\\makeatother

" ;;import de la feuille de syle dans texmf
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*a{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defgroup org-export-time nil
  "Options specific to Time back-end."
  :tag "Org Time PDF"
  :group 'ox-time)


(org-export-define-derived-backend 'time 'latex
  :options-alist
  '((:latex-class "LATEX_CLASS" nil "time" t)
    (:comp "COMP" nil "Delta" t)
    (:client "CLIENT" nil nil)
    (:contact "CONTACT" nil nil)
    (:fact "FACT" nil nil)
    (:tel "TEL" nil "0692 339108" t)
    (:adress "ADRESS" nil nil)
    (:email "EMAIL" nil nil t)
    (:with-toc nil "toc" 1 )
    (:latex-hyperref-p nil "texht" org-latex-with-hyperref t)
    (:logo "LOGO" nil " "))
  :translate-alist '((template . ox-time-template))
  :menu-entry
  '(?T "Export to Time layout"
       ((?l "As LaTeX file" ox-time-export-to-latex)
        (?p "As PDF file" ox-time-export-to-pdf)
        (?o "As PDF and Open"
            (lambda (a s v b)
              (if a (ox-time-export-to-pdf t s v b)
                (org-open-file (ox-time-export-to-pdf nil s v b))))))))

(defun ox-time-template (contents info)
  "INFO are the header data and CONTENTS is the content of the org file and return complete document string for this export."
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Créé le %d/%m/%Y %a %H:%M \n"))
   ;; Document class and packages.
   (let* ((class (plist-get info :latex-class))
          (class-options (plist-get info :latex-class-options))
          (header (nth 1 (assoc class org-latex-classes)))
          (document-class-string
           (and (stringp header)
                (if (not class-options) header
                  (replace-regexp-in-string
                   "^[\t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
                   class-options header t nil 1)))))
     (if (not document-class-string)
         (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
        (org-latex-guess-inputenc
         (org-element-normalize-string
          (org-splice-latex-header
           document-class-string
           org-latex-default-packages-alist ; Defined in org.el.
           org-latex-packages-alist nil     ; Defined in org.el.
           (concat (org-element-normalize-string (plist-get info :latex-header))
                   (plist-get info :latex-header-extra)))))
        info)))

   ;; Now the core content
   (let ((auteur (plist-get info :author))
         (titre (plist-get info :title)))
     (concat "

"(when (plist-get info :org-latex-with-hyperref)
   (format "{%s}" (plist-get info :org-latex-with-hyperref) ))"

%% content definition %<-------------------------------

\\title{"(org-export-data titre info)"}
\\date{\\today }
\\xdef\\invoicenum{"(when (plist-get info :fact)
   (format "%s" (plist-get info :fact) ))"}
\\xdef\\companyname{"(when (plist-get info :comp)
   (format "%s" (plist-get info :comp) ))"}
\\xdef\\companyaddress{"(when (plist-get info :adress)
   (format "%s" (plist-get info :adress) ))"}

\\xdef\\companytel{"(when (plist-get info :tel)
   (format "%s" (plist-get info :tel) ))"}
\\xdef\\companyemail{"(when (plist-get info :email)
                        (format "%s" (plist-get info :email) ))"}
\\xdef\\bankiban{XXXX\\,XXXX\\,XXXX\\,XXXX\\,XXXX\\,XXXX\\,XXXX}
\\xdef\\bankbic{XXX\\,XXX\\,XXX}
\\long\\xdef\\conditions{write the sell conditions here
on several lines}

\\makeatletter
\\let\\thetitle\\@title
\\makeatother

\\pagestyle{empty}

%% DOCUMENT %<-----------------------------------------
\\begin{document}

\\noindent\\begin{minipage}{0.3\\textwidth}%% adapt widths of minipages to your needs
\\includegraphics[height=70px,width=70px,keepaspectratio]{"(when (plist-get info :logo)
   (format "%s" (plist-get info :logo) ))"}
\\end{minipage}%%
\\hfill%
\\begin{minipage}{0.6\\textwidth}\\raggedleft

  \\ttfamily{
  {\\color{gray!95}\\fontsize{1.5cm}{1.5cm}\\selectfont %%
  \\vbox to 1cm{\\vss \\leavevmode \\kern -1mm
  Chronologie
  }}}

\\end{minipage}

\\kern -5mm

\\leavevmode\\kern \\rightalignment \\parbox{0.35\\textwidth}{\\ttfamily Facture Associée N\\textsuperscript{o} \\invoicenum\\\\
\\today }

\\vskip 0.7cm

\\leavevmode\\kern \\rightalignment\\kern -3mm \\colorbox{gray!85}{
  \\kern 1mm\\begin{minipage}[t]{0.5\\textwidth}
    \\color{white}
    \\vskip 2mm
    Client  : \\textbf{"(when (plist-get info :client)
   (format "%s" (plist-get info :client) ))"}\\\\
    Contact : \\textbf{"(when (plist-get info :contact)
   (format "%s" (plist-get info :contact) ))"}\\\\
    \\vspace*{-3mm}%%
  \\end{minipage}
}

\\vskip 2cm

\\begin{center}
{\\ttfamily\\LARGE \\thetitle}

\\rule{2cm}{0.25pt}
\\end{center}

" contents "

\\vfill

\\small

\\setlength{\\columnsep}{1.5cm}
\\begin{multicols}{2}
\\noindent\\companyname{}\\\\
\\companyaddress{}\\\\
\\hbox to 1cm{Tél\\,:\\hss} \\companytel\\\\
\\hbox to 1cm{Mél\\,:\\hss} \\companyemail\\\\
\\end{multicols}

\\end{document}
"))))


;;;###autoload
(defun ox-time-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Time (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'time outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-time-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Time (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'time file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun ox-time-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Time (pdf) and open.
If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'time outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-time)
;;; ox-time.el ends here
