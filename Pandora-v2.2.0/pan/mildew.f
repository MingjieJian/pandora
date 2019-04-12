      subroutine MILDEW
     $(NO,XNE,HND,N,POPK,POP,NL,KODE,LINE,LIMP,IOVER,PRINTN)
C
C     Rudolf Loeser, 1973 Jul 09
C---- Prints Population Densities.
C     See also PELLA.
C     (This is version 3 of MILDEW.)
C     !DASH
      save
C     !DASH
      real*8 HND, POP, POPK, XNE
      integer I, IOVER, J, JE, JS, KODE, LIMP, N, NL, NO
      logical PRINTN
      character LINE*9, MODE*9
C     !DASH
      external  LINER, KYNWYL, SHIM, HI, BYE
      intrinsic min
C
C               XNE(N), HND(N), POPK(N), POP(N,LIMP)
      dimension XNE(*), HND(*), POPK(*), POP(N,*)
C
      dimension LINE(*)
C
      call HI ('MILDEW')
C     !BEG
      call LINER (3,NO)
      write (NO,100)
  100 format(' ',41X,'N U M B E R   D E N S I T I E S')
C     !EJECT
      if(PRINTN) then
        call KYNWYL   (LINE,LIMP,KODE,MODE,IOVER,NL)
C
        JE = 0
  101   continue
          JS = JE+1
          JE = min(JE+8,LIMP)
C
          call LINER  (2,NO)
          write (NO,102) (J,J=JS,JE)
  102     format(' ',22X,'Total'/
     $           ' ',7X,'Electron',4X,'Hydrogen',3X,'Continuum',
     $               8(:,'   Level',I3))
          write (NO,103) MODE,(LINE(J),J=JS,JE)
  103     format(' ',3X,2(5X,'Density'),1X,9(2X,A9))
          call LINER  (1,NO)
C
          do 105 I = 1,N
            write (NO,104) I,XNE(I),HND(I),POPK(I),(POP(I,J),J=JS,JE)
  104       format(' ',I3,1P2E12.4,1X,9E11.3)
            call SHIM (I,5,NO)
  105     continue
C
        if(JE.lt.LIMP) goto 101
C
      else
        call LINER    (1,NO)
        write (NO,106)
  106   format(' ',41X,'NK and ND are all equal, to 8 figures, to the ',
     $             'values for the ion-of-the-run,'/
     $         ' ',41X,'which were already printed.')
      end if
C     !END
      call BYE ('MILDEW')
C
      return
      end
