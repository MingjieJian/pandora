      subroutine SPOKE
     $(NO,NMAX,KMAX,NT,JU,JL,KK,NN,XJNU,DL)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Prints restart Jnu values.
C     !DASH
      save
C     !DASH
      real*8 DL, XJNU
      integer I, JL, JU, KK, KMAX, NMAX, NN, NO, NT
      character TIT*45
C     !DASH
      external ABJECT, LINER, SULFUR, HI, BYE
C
C               XJNU(NMAX,KMAX,NONC), DL(KMAX,NONC), KK(NONC), NN(NONC),
      dimension XJNU(NMAX,KMAX,*),    DL(KMAX,*),    KK(*),    NN(*),
C
C               JU(NONC), JL(NONC)
     $          JU(*),    JL(*)
C
      data TIT(1:37) /'Restart values of Jnu for transition '/
C
      call HI ('SPOKE')
C     !BEG
      if(NO.gt.0) then
        call ABJECT   (NO)
C
        do 102 I = 1,NT
C
          call LINER  (3,NO)
          write (NO,100)
  100     format(' ',10('----------'))
          write (TIT(38:),101) JU(I),JL(I)
  101     format('(',I2,'/',I2,').')
C
          call SULFUR (NO,NN(I),KK(I),XJNU(1,1,I),DL(1,I),TIT)
  102   continue
C
      end if
C     !END
      call BYE ('SPOKE')
C
      return
      end
