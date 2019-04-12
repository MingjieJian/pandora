      subroutine GALENA
     $(IU,IL,K,L,INCRAD,NVY,WTAB,YLL,YUL,YNTN)
C
C     Rudolf Loeser, 1984 Sep 24
C---- Dumps for ROT.
C     (This is version 2 of GALENA.)
C     !DASH
      save
C     !DASH
      real*8 WTAB, YLL, YNTN, YUL
      integer I, IL, IU, J, K, L, LUEO, NVY
      logical INCRAD
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external HI, BYE
C
C               WTAB(KM), YNTN(KM,L)
      dimension WTAB(*),  YNTN(K ,*)
C
      call HI ('GALENA')
C     !BEG
      write (LUEO,100) IU,IL,K,L,INCRAD,NVY
  100 format(' ','(',I2,',',I2,'): K=',I3,', L=',I3,', INCRAD=',L2,
     $           ', NVY=',I2)
C
      write (LUEO,101) (WTAB(I),I=1,K)
  101 format(' ',1P10E12.4)
C
      write (LUEO,102) YLL,YUL
  102 format(' ','YLL=',1PE16.8,', YUL=',E16.8)
C
      do 104 J = 1,L
        write (LUEO,103) J
  103   format(' ',I5)
        write (LUEO,101) (YNTN(I,J),I=1,K)
  104 continue
C     !END
      call BYE ('GALENA')
C
      return
      end
