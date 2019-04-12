      subroutine GRADER
     $(IU,IL,XNE,TE,V,DW,FRCDL,FMCDL,FSTKM,CALLER)
C
C     Rudolf Loeser, 2002 Jul 10
C---- Prints, for TABOR.
C     !DASH
      save
C     !DASH
      real*8 DW, FMCDL, FRCDL, FSTKM, TE, V, XNE
      integer IL, IU, LUEO
      character CALLER*(*)
C     !DASH
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, LINER, HI, BYE
C
      call HI ('GRADER')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) IU,IL,XNE,TE,V,DW,FRCDL,FMCDL,FSTKM
  100 format(' ','Details from Stark splitting of the Hydrogen (',
     $           I2,'/',I2,') line.'//
     $       ' ','Ne =',1PE16.8,', Te =',E16.8,',V =',E14.6/
     $       ' ','DW =',E12.4,', FRCDL =',E12.4,', FMCDL =',E12.4,
     $           ', FSTKM =',E10.2)
      call LINER  (1, LUEO)
C     !END
      call BYE ('GRADER')
C
      return
      end
