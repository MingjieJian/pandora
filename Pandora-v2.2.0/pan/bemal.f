      subroutine BEMAL
     $(NO,KVLG)
C
C     Rudolf Loeser, 1999 Jan 07
C---- Prints velocity-related stuff, for PATCH.
C     !DASH
      save
C     !DASH
      real*8 CFH, CFHE
      integer I, KVLG, MDFV, NO
      character LAB*1
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
      equivalence (KZQ(106),MDFV )
      equivalence (RZQ(113),CFH  )
      equivalence (RZQ(152),CFHE )
C     !DASH
      external NIBBLE, LINER, HI, BYE
C
      dimension LAB(2)
C
      call HI ('BEMAL')
C     !BEG
      if(KVLG.gt.0) then
        write (NO,100) CFH,CFHE
  100   format(' ','Mass-flow parameters:  for Hydrogen CFH =',1PE12.4,
     $             ' ,  for Helium CFHE =',E12.4,' .')
        call LINER (1,NO)
      end if
C
      call NIBBLE  (LAB,(MDFV+1),1,2)
      write (NO,101) (LAB(I),I=1,2)
  101 format(' ','Writing the computed velocities to an output file ',
     $           'is controlled by MDFV:'/
     $       ' ','  MDFV      =  0',A,', they are not saved'/
     $       ' ','            =  1',A,', they are saved')
C     !END
      call BYE ('BEMAL')
C
      return
      end
