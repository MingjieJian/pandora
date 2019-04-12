      subroutine KROKANT
     $(N,OMD,IDDL)
C
C     Rudolf Loeser, 2004 Mar 12
C---- Checks the OMD table, to see whether the FULL solution can be
C     accepted.
C     !DASH
      save
C     !DASH
      real*8 DELLM, OMD, ZERO
      integer I, IDDL, N
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
      equivalence (RZQ(177),DELLM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               OMD(N)
      dimension OMD(*)
C
      call HI ('KROKANT')
C     !BEG
      IDDL = N
      if((DELLM.gt.ZERO).and.(OMD(IDDL).lt.DELLM)) then
C
        do 100 I = 2,N
          IDDL = IDDL-1
          if(OMD(IDDL).ge.DELLM) then
            goto 101
          end if
  100   continue
C
  101   continue
      end if
C     !END
      call BYE ('KROKANT')
C
      return
      end
