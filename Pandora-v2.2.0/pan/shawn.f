      subroutine SHAWN
     $(LU,ALOMI,ALOMA,ALB,TE,XNE,HND,WAVE,NKA,N,KWA,WAVCA,Z,ARRCA,LWNT)
C
C     Rudolf Loeser, 1993 Sep 16
C---- Prints data, for HERMAN.
C     !DASH
      save
C     !DASH
      real*8 ALB, ALOMA, ALOMI, ARRCA, HND, TE, WAVCA, WAVE, XNE, Z
      integer I, IE, IS, J, K, KWA, LU, LWNT, N, NKA
C     !DASH
      external  PRIAM, LINER, ACORUS, SHIM, HI, BYE
      intrinsic min
C
C               ALB(N), WAVCA(KWA), Z(N), ARRCA(KWA,N), HND(N), XNE(N),
      dimension ALB(*), WAVCA(*),   Z(*), ARRCA(KWA,*), HND(*), XNE(*),
C
C               TE(N)
     $          TE(*)
C
      call HI ('SHAWN')
C     !BEG
      if(LU.gt.0) then
        call PRIAM  (LU, 'AVERAGED', 8)
        call LINER  (1, LU)
        write (LU,100) LWNT,ALOMI,ALOMA
  100   format(' ','"Averaged" Line Opacity data',10X,'(LWNT=',I5,')',
     $             49X,'(Options AVELOP and AVOPRNT)'/
     $         ' ','Wavelength range:',1P2E16.8,' Angstroms')
        call ACORUS (LU, NKA, WAVE)
        call LINER  (1, LU)
C     !EJECT
        IE = 0
  101   continue
          IS = IE+1
          IE = min(IE+10,N)
          call LINER  (1, LU)
          write (LU,102) (I,I=IS,IE)
  102     format(' ',12X,'Depth',10I11)
          call LINER  (1, LU)
C
          write (LU,103) (Z(I),I=IS,IE)
  103     format(' ',16X,'Z',1P10E11.3)
          write (LU,104) '               TE',(TE(I) ,I=IS,IE)
          write (LU,104) '               NE',(XNE(I),I=IS,IE)
          write (LU,104) '               NH',(HND(I),I=IS,IE)
          write (LU,104) '           Albedo',(ALB(I),I=IS,IE)
  104     format(' ',A17,1P10E11.4)
          call LINER  (1, LU)
C
          write (LU,105)
  105     format(' ',3X,'Wavelength',5X,'O P A C I T I E S')
          call LINER  (1, LU)
C
          K = 0
          do 107 J = 1,KWA,LWNT
            K = K+1
            write (LU,106) WAVCA(J),(ARRCA(J,I),I=IS,IE)
  106       format(' ',1PE16.9,1X,10E11.4)
            call SHIM (K, 5, LU)
  107     continue
C
        if(IE.lt.N) goto 101
      end if
C     !END
      call BYE ('SHAWN')
C
      return
      end
