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
; Converts a schematic to a symbol
;
;Parameters
; sch               dbobj,  cellview of schematic
; sym               dbobj,  cellview of symbol
; scale             num, scaling
; leftMargin        num, left margin
; rightMargin       num, right margin
; topMargin         num, top margin
; bottomMargin      num, bottom margin
; stubLength        num, length of wire stubs at pins
; fontHeight        num, font height
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
      shapes tabPinPos delta pos dir halfheight halfwidth
      pinlabel pinpos sympin sympinMaster labelpos
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

      (setq tabPinPos (makeTable "tabPinPos" 0.0))

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

            (setarray tabPinPos dir 0.025)

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

          );foreach 
        );foreach
      );foreach


      (dbCreateRect
        sym
        (list "instance" "drawing")
        (list
          (list
            (plus (leftEdge sym~>bBox) (arrayref tabPinPos "left"))
            (plus (bottomEdge sym~>bBox) (arrayref tabPinPos "bottom"))    
          );list
          (list
            (difference (rightEdge sym~>bBox) (arrayref tabPinPos "right"))
            (difference (topEdge sym~>bBox) (arrayref tabPinPos "top"))    
          );list        
        );list
      );dbCreateRect

      t
    );when
  );let
);defun EDsch2sym