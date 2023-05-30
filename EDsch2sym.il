/*=============================================================================*
*
*   EDsch2sym
*
*
*   Purpose  : Convert a schematic in Cadence Virtuoso in a symbol
*   Authors  : Matthias Schweikardt
*
*   Language : Cadence Skill
*   Lint     : IQ score is 100 (best is 100) {sklint ?file "EDsch2sym.il"}
*  
*   Globals  :
*    - Functions :
*       o EDsch2sym
*       o _EDsch2SymCB
*       o _EDsch2SymOptionsCB
*
*    - Symbols   : none
*
*   Copyright 2023 Reutlingen University, Electronics & Drives (Germany)
*
*   Permission is hereby granted, free of charge, to any person obtaining a 
*   copy of this software and associated documentation files (the "Software"), 
*   to deal in the Software without restriction, including without limitation 
*   the rights to use, copy, modify, merge, publish, distribute, sublicense, 
*   and/or sell copies of the Software, and to permit persons to whom the 
*   Software is furnished to do so, subject to the following conditions:
*
*   The above copyright notice and this permission notice shall be included in 
*   all copies or substantial portions of the Software.
*
*   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
*   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
*   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
*   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
*   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
*   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
*   DEALINGS IN THE SOFTWARE.
*
*=============================================================================*/


;FUNCTION
; EDsch2sym
;
;Description:
; Converts a Schematic to a Symbol
;
;Parameters
; sch               dbobj,  cellview of schematic
; sym               dbobj,  cellview of symbol
; ?scale            num, scaling
; ?leftMargin       num, left margin
; ?rightMargin      num, right margin
; ?topMargin        num, top margin
; ?bottomMargin     num, bottom margin
; ?stubLength       num, length of wire stubs at pins
; ?fontHeight       num, font height
;
;Returns:
; t/nil    't' when successful, 'nil' otherwise
;
(defun EDsch2sym 
  (
    sch 
    sym 
    @key 
      (scale 1.0)
      (leftMargin 0.0)
      (rightMargin 0.0)
      (topMargin 0.0)
      (bottomMargin 0.0)
      (stubLength 0.25)
      (fontHeight 0.0625)
      (labelOffset 0.025)
  )

  (let 
    (
      shapes tabPinPosDelta delta pos dir halfheight halfwidth
      pinlabel pinpos sympin sympinMaster labelpos cdsTerm tabPinPos
    )

    (when
      (and
        (dbIsId sch)
        (dbIsId sym)
        (equal sch~>objType "cellView")
        (equal sym~>objType "cellView")
        (equal sch~>cellViewType "schematic")
        (equal sym~>cellViewType "schematicSymbol")
        (or
          (equal sym~>mode "w")
          (equal sym~>mode "a")
        );or
      );and

      (setq tabPinPos (makeTable "tabPinPos" nil))
      (setq tabPinPosDelta (makeTable "tabPinPosDelta" 0.0))

      (setq
        shapes
        (setof
          shape
          sch~>shapes
          (equal shape~>lpp (list "wire" "drawing"))
        );setof
      );setq

      (foreach shape shapes
        (when (equal shape~>objType "line")
          (dbCreateLine
            sym
            (list "device" "drawing")
            (dbTransformPointList shape~>points (list 0:0 "R0" scale))
          );dbCreateLine
        );when

        (when (equal shape~>objType "ellipse")
          (dbCreateEllipse
            sym
            (list "device" "drawing1") 
            (dbTransformBBox shape~>bBox (list 0:0 "R0" scale))
          );dbCreateEllipse
        );when
      );foreach

      (foreach inst sch~>instances

        (unless (equal inst~>master~>libName "basic")

          (setq
            shapes
            (setof
              shape
              inst~>master~>shapes
              (equal shape~>lpp (list "device" "drawing"))
            );setof
          );setq

          (foreach shape shapes

            (when (equal shape~>objType "line")
              (dbCreateLine
                sym
                (list "device" "drawing")
                (dbTransformPointList
                  (dbTransformPointList
                    shape~>points
                    inst~>transform
                  );dbTransformPointList
                  (list 0:0 "R0" scale)
                );dbTransformPointList
              );dbCreateLine
            );when

            (when (equal shape~>objType "ellipse")
              (dbCreateEllipse
                sym
                (list "device" "drawing")
                (dbTransformBBox
                  (dbTransformBBox
                    shape~>bBox
                    inst~>transform
                  );dbTransformPointList
                  (list 0:0 "R0" scale)
                );dbTransformPointList
              );dbCreateLine
            );when

            (when (equal shape~>objType "rect")
              (dbCreateRect
                sym
                (list "device" "drawing")
                (dbTransformBBox
                  (dbTransformBBox
                    shape~>bBox
                    inst~>transform
                  );dbTransformPointList
                  (list 0:0 "R0" scale)
                );dbTransformPointList
              );dbCreateRect
            );when

            (when (equal shape~>objType "arc")
              (dbCreateArc
                sym
                (list "device" "drawing")
                (dbTransformBBox
                  (dbTransformBBox
                    shape~>ellipseBBox
                    inst~>transform
                  );dbTransformPointList
                  (list 0:0 "R0" scale)
                );dbTransformPointList
                (dbTransformBBox
                  (dbTransformBBox
                    shape~>bBox
                    inst~>transform
                  );dbTransformPointList
                  (list 0:0 "R0" scale)
                );dbTransformPointList              
              );dbCreateLine
            );when
          );foreach
        );unless
      );foreach


      (dbCreateRect
        sym
        (list "device" "drawing")
        (list
          (list
            (difference (leftEdge sym~>bBox) leftMargin)
            (difference (bottomEdge sym~>bBox) bottomMargin)
          );list
          (list
            (plus (rightEdge sym~>bBox) rightMargin)
            (plus (topEdge sym~>bBox) topMargin)
          );list
        );list
      );dbCreateRect

      (setq
        sympinMaster
        (dbOpenCellViewByType
          "basic"
          "sympin"
          "symbolNN"
          "schematicSymbol"
        );dbOpenCellViewByType
      );setq

      (setq
        halfwidth
        (quotient 
          (difference (rightEdge sym~>bBox) (leftEdge sym~>bBox))
          2.0
        );quotient
      );setq

      (setq
        halfheight
        (quotient 
          (difference (topEdge sym~>bBox) (bottomEdge sym~>bBox))
          2.0
        );quotient
      );setq

      (foreach terminal sch~>terminals

        (foreach pin terminal~>pins

          (foreach fig pin~>figs

            (setq pos (dbTransformPoint fig~>xy (list 0:0 "R0" scale)))

            (setq
              delta
              (list
                (difference (xCoord pos) (xCoord (centerBox sym~>bBox)))
                (difference (yCoord pos) (yCoord (centerBox sym~>bBox)))
              );list
            );setq

            (setq
              dir
              (if (greaterp (xCoord delta) 0)
                (if (greaterp (yCoord delta) 0) 
                  (if 
                    (lessp 
                      (difference halfwidth (xCoord delta))
                      (difference halfheight (yCoord delta))
                    );lessp
                    "right"
                    "top"
                  );if
                  (if 
                    (lessp 
                      (difference halfwidth (xCoord delta))
                      (plus halfheight (yCoord delta))
                    );lessp
                    "right"
                    "bottom"
                  );if
                );if
                (if (greaterp (yCoord delta) 0) 
                  (if 
                    (lessp 
                      (plus halfwidth (xCoord delta))
                      (difference halfheight (yCoord delta))
                    );lessp
                    "left"
                    "top"
                  );if
                  (if 
                    (lessp 
                      (plus halfwidth (xCoord delta))
                      (plus halfheight (yCoord delta))
                    );lessp
                    "left"
                    "bottom"
                  );if
                );if
              );if
            );setq

            (setarray tabPinPosDelta dir 0.025)

            (setq
              pinpos
              (if (greaterp stubLength 0)
                (case dir
                  ("top" 
                    (list (xCoord pos) (plus (yCoord pos) stubLength))
                  )
                  ("bottom"
                    (list (xCoord pos) (difference (yCoord pos) stubLength))
                  )
                  ("left" 
                    (list (difference (xCoord pos) stubLength) (yCoord pos))
                  )             
                  (t
                    (list (plus (xCoord pos) stubLength) (yCoord pos))
                  )
                );case
                pos
              );if
            );setq

            (setq
              labelpos
              (if (greaterp stubLength 0)
                (case dir
                  ("top" 
                    (list
                      (plus (xCoord pos) labelOffset)
                      (plus (yCoord pos) (times 0.5 stubLength))
                    );list
                  )
                  ("bottom"
                    (list
                      (plus (xCoord pos)  labelOffset)
                      (difference (yCoord pos) (times 0.5 stubLength))
                    );list
                  )
                  ("left" 
                    (list
                      (difference (xCoord pos) (times 0.5 stubLength))
                      (plus (yCoord pos) labelOffset)
                    );list
                  )             
                  (t
                    (list
                      (plus (xCoord pos) (times 0.5 stubLength))
                      (plus (yCoord pos) labelOffset)
                    );list
                  )
                );case
                pos
              );if
            );setq


            (unless (equal pos pinpos)
              (dbCreateLine sym (list "device" "drawing") (list pos pinpos))
            );unless
            
            (setq
              sympin
              (schCreateSymbolPin
                sym
                sympinMaster
                terminal~>name
                terminal~>direction
                pinpos
                "R0"
              );schCreateSymbolPin
            );setq

            (setq
              pinlabel
              (schCreateSymbolLabel
                sym
                labelpos 
                "pin name"
                terminal~>name
                (case dir
                  ("top" "upperCenter")
                  ("bottom" "upperCenter")
                  ("left" "lowerCenter")            
                  (t "lowerCenter")
                );case
                (case dir
                  ("top" "R90")
                  ("bottom" "R0")
                  ("left" "R0")            
                  (t "R90")
                );case
                "stick"
                fontHeight
                "normalLabel"
              );schCreateSymbolLabel
            );setq

           
            (schGlueLabel pinlabel sympin)
            (setarray tabPinPos fig (list pinpos sympin dir))

          );foreach 
        );foreach
      );foreach


      (dbCreateRect
        sym
        (list "instance" "drawing")
        (list
          (list
            (plus (leftEdge sym~>bBox) (arrayref tabPinPosDelta "left"))
            (plus (bottomEdge sym~>bBox) (arrayref tabPinPosDelta "bottom"))    
          );list
          (list
            (difference (rightEdge sym~>bBox) (arrayref tabPinPosDelta "right"))
            (difference (topEdge sym~>bBox) (arrayref tabPinPosDelta "top"))    
          );list        
        );list
      );dbCreateRect

      (foreach terminal sch~>terminals

        (foreach pin terminal~>pins

          (foreach fig pin~>figs

            (setq pinpos (car (arrayref tabPinPos fig)))
            (setq sympin (cadr (arrayref tabPinPos fig)))
            (setq dir (caddr (arrayref tabPinPos fig)))

            (setq
              cdsTerm
              (schCreateSymbolLabel
                sym
                pinpos 
                "analog pin annotate"
                (lsprintf "cdsTerm(\"%s\")" terminal~>name)
                (case dir
                  ("top" "upperLeft")
                  ("bottom" "lowerRight")
                  ("left" "upperRight")            
                  (t "upperLeft")
                );case
                (case dir
                  ("top" "R90")
                  ("bottom" "R90")
                  ("left" "R0")
                  (t "R0")
                );case
                "stick"
                fontHeight
                "normalLabel"
              );schCreateSymbolLabel
            );setq

             (schGlueLabel cdsTerm sympin)

          );foreach 
        );foreach
      );foreach
      t
    );when
  );let
);defun EDsch2sym


;INTERNAL FUNCTION
; _EDsch2SymOptionsCB
;
;Description:
; This callback is triggered when the Options Disclosure Button is pushed 
; on the form
;
(defun _EDsch2SymOptionsCB ()
  (let (form)

    (setq form (eval (quote EDsch2SymForm)))

    (unless form->discButton->value
      (hiSetFormSize form (list (car (hiGetFormSize form)) 201))
    );unless
  );let
);defun _EDsch2SymOptionsCB


;INTERNAL FUNCTION
; _EDsch2SymCB
;
;Description:
; This callback is triggered 'OK' or 'Apply' is pushed on the form
;
(defun _EDsch2SymCB ()
  (let (form sch sym)
    (setq form (eval (quote EDsch2SymForm)))

    (if 
      (muffleWarnings
        (ddGetObj
          form->libSchematicCombo->value
          form->cellSchematicCombo->value
          form->viewSchematicCombo->value
          "sch.oa"
        );ddGetObj
      );muffleWarnings
    then
      (setq
        sch
        (dbOpenCellViewByType
          form->libSchematicCombo->value
          form->cellSchematicCombo->value
          form->viewSchematicCombo->value
          "schematic"
          "r"
        );dbOpenCellViewByType
      );setq

      (if
        (muffleWarnings
          (ddGetObj
            form->libSymbolCombo->value
          );ddGetObj
        );muffleWarnings
      then

        (setq
          sym
          (dbOpenCellViewByType
            form->libSymbolCombo->value
            form->cellSymbolCombo->value
            form->viewSymbolCombo->value
            "schematicSymbol"
            "w"
          );dbOpenCellViewByType
        );setq

        (EDsch2sym
          sch
          sym
          ?scale         form->scalingFloatField->value
          ?leftMargin    form->leftMarginFloatField->value
          ?rightMargin   form->rightMarginFloatField->value
          ?topMargin     form->topMarginFloatField->value
          ?bottomMargin  form->bottomMarginFloatField->value
          ?stubLength    form->stubLengthFloatField->value
          ?fontHeight    form->fontHeightFloatField->value
          ?labelOffset   form->labelOffsetFloatField->value               
        );EDsch2sym

        (dbSave sym)
        (dbClose sym)
      else
        (error 
          "\"%s\" is not a library" 
          form->libSymbolCombo->value  
        );error
      );if

      (dbClose sch)

    else
      (error 
        "\"%s/%s/%s\" is not a schematic cellview" 
        form->libSchematicCombo->value
        form->cellSchematicCombo->value    
        form->viewSchematicCombo->value    
      );error
    );if
  );let
);defun _EDsch2SymCB

(let 
  (
    libSchematicCombo cellSchematicCombo viewSchematicCombo schematicFormLayout
    libSymbolCombo cellSymbolCombo viewSymbolCombo symbolFormLayout
    bottomMarginFloatField fontHeightFloatField labelOffsetFloatField 
    leftMarginFloatField rightMarginFloatField stubLengthFloatField
    topMarginFloatField optionsFormLayout discButton
    horizontalBoxLayout scalingFloatField verticalBoxLayout form
  )
 
  (setq
    libSchematicCombo
    (ddHiCreateLibraryComboField
      ?name (quote libSchematicCombo) 
      ?prompt "Library Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateLibraryComboField
  );setq

  (setq
    cellSchematicCombo
    (ddHiCreateCellComboField
      ?name (quote cellSchematicCombo) 
      ?prompt "Cell Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateCellComboField
  );setq

  (setq
    viewSchematicCombo
    (ddHiCreateViewComboField
      ?name (quote viewSchematicCombo)
      ?prompt "View Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateViewComboField
  );setq

  (setq
    schematicFormLayout
    (hiCreateFormLayout
      (quote schematicFormLayout)
      ?frame "Schematic"
      ?items
        (list
          libSchematicCombo
          cellSchematicCombo
          viewSchematicCombo
        );list
    );hiCreateFormLayout
  );setq

  (setq
    libSymbolCombo
    (ddHiCreateLibraryComboField
      ?name (quote libSymbolCombo) 
      ?prompt "Library Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateLibraryComboField
  );setq

  (setq
    cellSymbolCombo
    (ddHiCreateCellComboField
      ?name (quote cellSymbolCombo) 
      ?prompt "Cell Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateCellComboField
  );setq

  (setq
    viewSymbolCombo
    (ddHiCreateViewComboField
      ?name (quote viewSymbolCombo) 
      ?prompt "View Name"
      ?callback "(ddsUpdateSyncWithForm)"
    );ddHiCreateViewComboField
  );setq

  (setq
    symbolFormLayout
    (hiCreateFormLayout
      (quote symbolFormLayout)
      ?frame "Symbol"
      ?items
        (list
          libSymbolCombo
          cellSymbolCombo
          viewSymbolCombo
        );list
    );hiCreateFormLayout
  );setq

  (setq
    horizontalBoxLayout
    (hiCreateHorizontalBoxLayout
      (quote horizontalBoxLayout)
      ?items (list schematicFormLayout symbolFormLayout)
    );hiCreateHorizontalBoxLayout
  );setq

  (setq
    scalingFloatField
    (hiCreateFloatField
      ?name (quote scalingFloatField)
      ?value 1.0
      ?prompt "Scaling"
      ?range (list 0.05 nil)
    );hiCreateFloatField
  );setq

  (setq
    leftMarginFloatField
    (hiCreateFloatField
      ?name (quote leftMarginFloatField)
      ?value 0.0
      ?prompt "Left Margin"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    rightMarginFloatField
    (hiCreateFloatField
      ?name (quote rightMarginFloatField)
      ?value 0.0
      ?prompt "Right Margin"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    topMarginFloatField
    (hiCreateFloatField
      ?name (quote topMarginFloatField)
      ?value 0.0
      ?prompt "Top Margin"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    bottomMarginFloatField
    (hiCreateFloatField
      ?name (quote bottomMarginFloatField)
      ?value 0.0
      ?prompt "Bottom Margin"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    stubLengthFloatField
    (hiCreateFloatField
      ?name (quote stubLengthFloatField)
      ?value 0.25
      ?prompt "Stub Length"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    fontHeightFloatField
    (hiCreateFloatField
      ?name (quote fontHeightFloatField)
      ?value 0.0625
      ?prompt "Font Height"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    labelOffsetFloatField
    (hiCreateFloatField
      ?name (quote labelOffsetFloatField)
      ?value 0.025
      ?prompt "Label Offset"
      ?range (list 0.0 nil)
    );hiCreateFloatField
  );setq

  (setq
    optionsFormLayout
    (hiCreateFormLayout
      (quote optionsFormLayout)
      ?frame "Options"
      ?items
        (list
          scalingFloatField
          leftMarginFloatField
          rightMarginFloatField
          topMarginFloatField
          bottomMarginFloatField
          stubLengthFloatField
          fontHeightFloatField
          labelOffsetFloatField
        );list
      ?invisible t
    );hiCreateVerticalBoxLayout
  );setq

  (setq 
    discButton
    (hiCreateDisclosureButton
      ?name 'discButton
      ?onFields '(optionsFormLayout)
      ?callback "(_EDsch2SymOptionsCB)"
    );hiCreateDisclosureButton
  );setq

  (setq
    verticalBoxLayout
    (hiCreateVerticalBoxLayout
      (quote verticalBoxLayout)
      ?items 
        (list
          horizontalBoxLayout
          discButton
          optionsFormLayout
        );list
    );hiCreateVerticalBoxLayout
  );setq

  (setq
    form
    (hiCreateLayoutForm
      (quote EDsch2SymForm)
      "[ED] Schematic To Symbol"
      verticalBoxLayout
      ?sizePolicy (quote horizontalExpanding)
      ?dialogStyle (quote modeless)
      ?buttonLayout (quote OKCancelApply)
      ?minSize 600:100
      ?callback
        (list
          "(_EDsch2SymCB)"
          "nil"
        );list
      ?buttonDisabled (list (quote Help))
    );hiCreateLayoutForm
  );setq

  (hiInstantiateForm form)

  (ddHiLinkFields
    form->libSchematicCombo  
    form->cellSchematicCombo
    form->viewSchematicCombo
  );ddHiLinkFields

  (ddHiLinkFields
    form->libSymbolCombo  
    form->cellSymbolCombo
    form->viewSymbolCombo
  );ddHiLinkFields
);let