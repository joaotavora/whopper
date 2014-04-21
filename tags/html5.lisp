(in-package :whopper)


;;; Changed elements

(def-empty-html-tag <:meta :i18n
  charset
  content
  http-equiv
  name
  scheme)

(def-html-tag <:input :core :event :i18n
  accept align alt autocomplete autofocus
  checked disabled form formaction formenctype formmethod
  formnovalidate formtarget height list max maxlength min multiple
  name pattern placeholder readonly required size src step type
  value width)



;;; New elements
;; Defines graphic drawing using JavaScript
(def-html-tag <:canvas :core :event :i18n
  height width)

;; Defines sound or music content
(def-html-tag <:audio :core :event :i18n
  autoplay controls loop muted preload src)

;; Defines containers for external applications (like plug-ins)
(def-html-tag <:embed :core :event :i18n
  height src type width)

;; Defines sources for <video> and <audio>
(def-html-tag <:source :core :event :i18n
  media src type)

;; Defines tracks for <video> and <audio>
(def-html-tag <:track :core :event :i18n
  default kind label src srclang)

;; Defines video or movie content
(def-html-tag <:video :core :event :i18n
  autoplay controls height loop muted poster preload src width)

;; Defines pre-defined options for input controls
(def-html-tag <:datalist :core :event :i18n)

;; Defines a key-pair generator field (for forms)
(def-html-tag <:keygen :core :event :i18n
  autofocus disabled form keytype name)

;; Defines the result of a calculation
(def-html-tag <:output :core :event :i18n
  for form name)

;; Defines an article in the document
(def-html-tag <:article :core :event :i18n)

;; Defines content aside from the page content
(def-html-tag <:aside :core :event :i18n)

;; Defines a part of text that might be formatted in a different
;; direction from other text outside it
(def-html-tag <:bdi :core :event :i18n)

;; Defines additional details that the user can view or hide
(def-html-tag <:details :core :event :i18n
  open)

;; Defines a dialog box or window
(def-html-tag <:dialog :core :event :i18n
  open)

;; Defines a caption for a <figure> element
(def-html-tag <:figcaption :core :event :i18n)

;; Defines self-contained content, like illustrations, diagrams,
;; photos, code listings, etc.
(def-html-tag <:figure :core :event :i18n)

;; Defines a footer for the document or a section
(def-html-tag <:footer :core :event :i18n)

;; Defines a header for the document or a section
(def-html-tag <:header :core :event :i18n)

;; Defines the main content of a document
(def-html-tag <:main :core :event :i18n)

;; Defines marked or highlighted text
(def-html-tag <:mark :core :event :i18n)

;; Defines a command/menu item that the user can invoke from a popup
;; menu
(def-html-tag <:menuitem :core :event :i18n
  checked default disabled icon label radiogroup type)

;; Defines a scalar measurement within a known range (a gauge)
(def-html-tag <:meter :core :event :i18n
  form high low max min optimum value)

;; Defines navigation links in the document
(def-html-tag <:nav :core :event :i18n)

;; Defines the progress of a task
(def-html-tag <:progress :core :event :i18n
  max value)

;; Defines what to show in browsers that do not support ruby
;; annotations
(def-html-tag <:rp :core :event :i18n)

;; Defines an explanation/pronunciation of characters (for East Asian
;; typography)
(def-html-tag <:rt :core :event :i18n)

;; Defines a ruby annotation (for East Asian typography)
(def-html-tag <:ruby :core :event :i18n)

;; Defines a section in the document
(def-html-tag <:section :core :event :i18n)

;; Defines a visible heading for a <details> element
(def-html-tag <:summary :core :event :i18n)

;; Defines a date/time
(def-html-tag <:time :core :event :i18n
  datetime)

;; Defines a possible line-break
(def-html-tag <:wbr :core :event :i18n)


