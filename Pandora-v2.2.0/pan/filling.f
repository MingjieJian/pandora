      subroutine FILLING
     $(TITLE,TNU,N,TNP,K,TSMLL,TLRGE,TBAR,KASE,FIN,IS,IL,IB)
C
C     Rudolf Loeser, 1989 Nov 02
C---- Prints debug details for the reduced-TAU procedures
C     of the WN-matrix calculations.
C     (This is version 4 of FILLING.)
C     !DASH
      save
C     !DASH
      real*8 TBAR, TLRGE, TNP, TNU, TSMLL
      integer IB, IL, IS, K, KASE, LUEO, N
      logical FIN
      character LABEL*9, TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, VECOUT, HI, BYE
C
C               TNU(N), TNP(N)
      dimension TNU(*), TNP(*)
C
      dimension LABEL(4)
C
      data LABEL /'QR-direct', 'QR-mapped', 'RT       ', 'GR       '/
C     !EJECT
C
      call HI ('FILLING')
C     !BEG
      write (LUEO,100) KASE,LABEL(KASE),TITLE
  100 format(' ','Details of reduced-TAU procedure for WN-matrix, ',
     $           'KASE =',I2,': ',A9/
     $       ' ',A)
      call VECOUT   (LUEO, TNU, N, 'Original TAU-table, TNU')
C
      if((KASE.eq.1).or.(KASE.eq.2).or.(KASE.eq.3)) then
        call LINER  (1, LUEO)
        write (LUEO,102) TSMLL,IS
  102   format(' ','TSMALL =',1PE10.3,3X,'IS =',I3)
        if(KASE.eq.3) then
          write (LUEO,103) FIN
  103     format(' ','FINITE =',L4)
        end if
      end if
C
      if((KASE.eq.1).or.(KASE.eq.2)) then
        call LINER  (1, LUEO)
        write (LUEO,104) TLRGE,IL
  104   format(' ','TLARGE =',1PE10.3,3X,'IL =',I3)
      end if
C
      if(K.le.0) then
        call LINER  (1, LUEO)
        write (LUEO,105) K
  105   format(' ','Reduced TAU-table length K =' I5)
      else
        call VECOUT (LUEO, TNP, K, 'Reduced TAU-table, TNP')
      end if
C
      if(KASE.eq.2) then
        call LINER  (1, LUEO)
        write (LUEO,107) TBAR,IB
  107   format(' ','TBAR =',1PE10.3,3X,'IB =',I3)
      end if
C     !END
      call BYE ('FILLING')
C
      return
      end
