      subroutine BAKAIRI
     $(IMAGE,DL,ZTM,K,INDX,VALU)
C
C     Rudolf Loeser, 1992 Dec 30
C---- Enters points into graph image, for BROOM.
C     (This is version 2 of BAKAIRI.)
C     !DASH
      save
C     !DASH
      real*8 DD, DL, DM, ZTM
      integer I, J, K
      logical INDX, VALU
      character IMAGE*(*), STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
C     !DASH
      external KLINEC, KPLOTC, HI, BYE
C
C               DL(K), ZTM(KM,4)
      dimension DL(*), ZTM(K ,*)
C     !EJECT
C
      call HI ('BAKAIRI')
C     !BEG
      do 101 J = 2,4
C
        do 100 I = 2,K
C
          if(INDX) then
            DD = I
            DM = I-1
          else if(VALU) then
            DD = DL(I)
            DM = DL(I-1)
          end if
C
          call KLINEC (IMAGE,DD,ZTM(I,J),DM,ZTM(I-1,J),ALPHS(J-1),1)
  100   continue
C
  101 continue
C
      do 102 I = 1,K
C
        if(INDX) then
          DD = I
        else if(VALU) then
          DD = DL(I)
        end if
C
        call KPLOTC   (IMAGE,DD,ZTM(I,1),STAR)
  102 continue
C     !END
      call BYE ('BAKAIRI')
C
      return
      end
