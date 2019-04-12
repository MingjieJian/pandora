      subroutine LAPWING
     $(NO,IU,IL,LDL,NVX)
C
C     Rudolf Loeser, 1985 Jun 20
C---- Prints heading for Profile Analysis.
C     !DASH
      save
C     !DASH
      integer IL, IU, LDL, NANA1, NANA2, NO, NVX
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(135),NANA1)
      equivalence (KZQ(137),NANA2)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('LAPWING')
C     !BEG
      if(NO.gt.0) then
        if(LDL.gt.1) then
          write (NO,100) IU,IL,LDL
  100     format(' ','Details of Absorption Profile for Line (',I2,
     $               '/',I2,'): blend of ',I2,' components.')
        else
          write (NO,101) IU,IL
  101     format(' ','Details of Absorption Profile for Line (',I2,
     $               '/',I2,'): single line.')
        end if
C
        if(NVX.gt.0) then
          write (NO,102) NVX
  102     format(' ',11X,'Expansion Velocity set #',I3,'.')
        end if
C
        call LINER (1,NO)
        write (NO,103) NANA1,NANA2
  103   format(' ','Depths selection parameters: NANAL1 =',I4,5X,
     $             'NANAL2 =',I4)
        call LINER (1,NO)
      end if
C     !END
      call BYE ('LAPWING')
C
      return
      end
