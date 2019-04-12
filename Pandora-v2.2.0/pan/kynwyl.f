      subroutine KYNWYL
     $(LINE,LIMP,KODE,MODE,IOVER,NL)
C
C     Rudolf Loeser, 1974 Jul 09
C---- Sets up labels for printing of populations data.
C     !DASH
      save
C     !DASH
      integer IOVER, KODE, LAMB, LIMP, NL
      character COM*9, INP*9, LINE*9, MODE*9, NNO*9, NYS*9
C     !DASH
      external  MARKI, SETC, HI, BYE
      intrinsic min
C
C               LINE(LIMPLIM)
      dimension LINE(*)
C
      data NNO,COM,INP /'(approx.)','  (comp.)','  (input)'/
C
      call HI ('KYNWYL')
C     !BEG
      call MARKI  (IOVER,0,NYS,INP,COM)
      call MARKI  (KODE,1,MODE,NYS,NNO)
C
      LAMB = min(NL,LIMP)
      if(LAMB.gt.0) then
        call SETC (LINE,1,LAMB,NYS)
      end if
C
      LAMB = LAMB+1
      if(LAMB.le.LIMP) then
        call SETC (LINE(LAMB),1,(LIMP-LAMB+1),NNO)
      end if
C     !END
      call BYE ('KYNWYL')
C
      return
      end
