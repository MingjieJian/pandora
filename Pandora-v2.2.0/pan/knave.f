      subroutine KNAVE
     $(XNUU,FEFF)
C
C     Rudolf Loeser, 2005 Feb 23
C---- Computes a term needed for default CVW and CSK
C     !DASH
      save
C     !DASH
      real*8 FAC, FEFF, FIVE, XNUK, XNUU
      integer IONST
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
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ( 56),IONST)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 6),FIVE  )
C     !DASH
      external HI, BYE
C
      data FAC /3.288D0/
C
      call HI ('KNAVE')
C     !BEG
      if(XNUU.ge.XNUK) then
        FEFF = FIVE
      else
        FEFF = IONST**2
        FEFF = (FAC*FEFF)/(XNUK-XNUU)
      end if
C     !END
      call BYE ('KNAVE')
C
      return
      end
