      subroutine JACANA
     $(NO)
C
C     Rudolf Loeser, 1985 Dec 16
C---- Prints a heading for CORAL.
C     !DASH
      save
C     !DASH
      real*8 PRTLM
      integer NO
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
      equivalence (RZQ( 47),PRTLM)
C     !DASH
      external HI, BYE
C
      call HI ('JACANA')
C     !BEG
      if(NO.gt.0) then
        write (NO,100) PRTLM
  100   format(' ','Partition Function: edited*, so that, for a given ',
     $             'ion, no value exceeds PARTLIM times the minimum.'/
     $         ' ','PARTLIM =',1PE10.3,10X,'(Edited values have an ',
     $             'appended asterisk.)')
      end if
C     !END
      call BYE ('JACANA')
C
      return
      end
