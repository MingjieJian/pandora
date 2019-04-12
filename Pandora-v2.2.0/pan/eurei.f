      subroutine EUREI
     $(NO,N,L,JLEV,TKRJ,RKCJ,TAUJ,AKJ,RK)
C
C     Rudolf Loeser, 1984 Jan 20
C---- Prints data for level JLEV, for ZEPELIN.
C     !DASH
      save
C     !DASH
      real*8 AKJ, RK, RKCJ, TAUJ, TKRJ
      integer I, IE, IS, JLEV, K, L, N, NO
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               TKRJ(L), RKCJ(L), TAUJ(N,L), AKJ(N,L), RK(N)
      dimension TKRJ(*), RKCJ(*), TAUJ(N,*), AKJ(N,*), RK(N)
C
      call HI ('EUREI')
C     !BEG
      call LINER   (3,NO)
      write (NO,100) JLEV
  100 format(' ','******* Level',I3)
C
      IE = 0
  101 continue
        IS = IE+1
        IE = min(IE+8,N)
C
        call LINER (2,NO)
        write (NO,102) (I,I=IS,IE)
  102   format(' ',3X,'Wavelength',2X,'Multiplier',22X,8I10)
        call LINER (1,NO)
C
        do 105 K = 1,L
          write (NO,103) TKRJ(K),RKCJ(K),(TAUJ(I,K),I=IS,IE)
  103     format(' ',1PE14.6,E10.2,5X,'Optical Depth',5X, 8E10.3)
          write (NO,104)                 (AKJ(I,K), I=IS,IE)
  104     format(' ',29X,1P,'Ionization',8X,              8E10.3)
  105   continue
C
        call LINER (1,NO)
        write (NO,106)                   (RK(I),    I=IS,IE)
  106   format(' ',16X,1P,'Total Photoionization rate',5X,8E10.3)
C
      if(IE.lt.N) goto 101
C     !END
      call BYE ('EUREI')
C
      return
      end
