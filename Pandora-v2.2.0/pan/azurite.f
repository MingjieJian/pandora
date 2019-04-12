      subroutine AZURITE
     $(NO,HYDR)
C
C     Rudolf Loeser, 1999 Nov 08
C---- Prints explanation of the total cooling rate calculation for
C     Hydrogen runs.
C     (This is version 2 of AZURITE.)
C     !DASH
      save
C     !DASH
      integer I, IN, KHFFS, KOOLS, NO
      logical HYDR
      character LAB*3
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
      equivalence (KZQ( 86),KOOLS)
      equivalence (KZQ(175),KHFFS)
C     !DASH
      external MERGINI, LINER, HI, BYE
C
      dimension IN(6), LAB(2)
C
      data LAB /'no ', 'yes'/
C     !EJECT
C
      call HI ('AZURITE')
C     !BEG
      if(HYDR) then
        call MERGINI (HYDR,IN)
        do 100 I = 1,6
          IN(I) = IN(I)+1
  100   continue
C
        call LINER   (2,NO)
        write (NO,101) KOOLS,KHFFS,LAB(IN(6)),LAB(IN(1)),LAB(IN(2)),
     $                 LAB(IN(3)),LAB(IN(4)),LAB(IN(5))
  101   format(' ','This is a HYDROGEN run, with KOOLSUM =',I3,
     $             ' and KHFFS =',I2//
     $         ' ','Besides H(bound-free) and H(bound-bound), the ',
     $             'following are included in the subtotal labelled ',
     $             'TOTAL H -'//
     $         ' ','        H(free-free): ',A//
     $         ' ','Besides TOTAL H, the following are included in ',
     $             'the grand total labelled TOTAL RATE -'//
     $         ' ','    H- (if computed): ',A/
     $         ' ','Conduction (general): ',A/
     $         ' ','     Composite Lines: ',A/
     $         ' ','               X-ray: ',A/
     $         ' ','            CO-lines: ',A)
C
      end if
C     !END
      call BYE ('AZURITE')
C
      return
      end
