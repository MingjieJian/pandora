      subroutine CUEL
     $(IL,TE,XNC,CQL)
C
C     Rudolf Loeser, 1994 May 05
C---- Computes the quantity q(l), to account for the reduction of the
C     ionization limit when calculating A(u,l), CI(l), and CE(u,l)
C     for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 CQL, EL2, FAC, ONE, RAT, ROOT, TE, XNC, ZERO
      integer IL, IXNCS
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
      equivalence (KZQ(144),IXNCS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external DIVIDE, HALT, HI, BYE
C
      data FAC /1.53446D-9/
C
      call HI ('CUEL')
C     !BEG
      if((XNC.le.ZERO).or.(IXNCS.le.0)) then
        CQL = ONE
      else
C
        call DIVIDE (XNC,TE,RAT)
        ROOT = sqrt(RAT)
        EL2  = IL**2
        CQL  = ONE/(ONE-(FAC*EL2*ROOT))
C
        if(CQL.eq.ZERO) then
          write (MSSLIN(1),100) IL,TE,XNC,CQL
  100     format(' ','l =',I12,', TE =',1PE24.16,', NC =',E24.16,
     $               'q(l) =',E12.4)
          write (MSSLIN(2),101)
  101     format(' ','Gene Avrett claims this is not possible.',10X,
     $               '2003 Feb 20')
          call HALT  ('CUEL',2)
        end if
C
      end if
C     !END
      call BYE ('CUEL')
C
      return
      end
