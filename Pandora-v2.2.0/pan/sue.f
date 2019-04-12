      subroutine SUE
     $(NO,CURMI,CURMA,ALBK,WAVEK,TE,XNE,HND,D,ARRK,KURIN,FKUR,WAVE,N,
     $ NKA,KNW,LWNT)
C     Rudolf Loeser, 1973 Jul 24
C---- Prints data, for ARIL.
C     !DASH
      save
C     !DASH
      real*8 ALBK, ARRK, CURMA, CURMI, D, FKUR, HND, TE, WAVE, WAVEK,
     $       XNE
      integer I, IE, IS, J, K, KNW, KURIN, LWNT, N, NKA, NO
C     !DASH
      external  PRIAM, ACORUS, LINER, SHIM, HI, BYE
      intrinsic min
C
C               WAVEK(KNW), HND(N), XNE(N), D(N), ALBK(N), ARRK(KNW,N),
      dimension WAVEK(*),   HND(*), XNE(*), D(*), ALBK(*), ARRK(KNW,*),
C
C               FKUR(KNW), TE(N)
     $          FKUR(*),   TE(*)
C
      call HI ('SUE')
C     !BEG
      if(NO.gt.0) then
        call PRIAM    (NO,'STATISTICAL',11)
        call LINER    (1,NO)
        write (NO,100) LWNT,KURIN,CURMI,CURMA
  100   format(' ','Kurucz''s Statistical Line Blanketing data',10X,
     $             '(LWNT=',I3,')'//
     $         ' ','K     =',I3/
     $         ' ','LLMIN =',F6.0/
     $         ' ','LLMAX =',F6.0)
        call ACORUS   (NO,NKA,WAVE)
        call LINER    (1,NO)
C     !EJECT
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+9,N)
          call LINER  (1,NO)
          write (NO,102) (I,I=IS,IE)
  102     format(' ',14X,'Depth',9I12)
          call LINER  (1,NO)
C
          write (NO,103) (TE(I),I=IS,IE)
  103     format(' ',9X,'TE',8X,9F12.0)
          write (NO,104) 'NE        ',(XNE(I),I=IS,IE)
  104     format(' ',9X,A10,1P9E12.5)
          write (NO,104) 'NH        ',(HND(I),I=IS,IE)
          write (NO,104) 'D         ',(D(I)  ,I=IS,IE)
          write (NO,105) (ALBK(I),I=IS,IE)
  105     format(' ',9X,'Albedo',4X,9F12.5)
          call LINER  (1,NO)
C
          write (NO,106)
  106     format(' ','Wavelength',3X,'Mult',3X,'O P A C I T I E S')
          call LINER  (1,NO)
C
          K = 0
          do 108 J = 1,KNW,LWNT
            K = K+1
            write (NO,107) WAVEK(J),FKUR(J),(ARRK(J,I),I=IS,IE)
  107       format(' ',F10.2,1PE9.2,9E12.5)
            call SHIM (K,5,NO)
  108     continue
C
        if(IE.lt.N) goto 101
      end if
C     !END
      call BYE ('SUE')
C
      return
      end
