      subroutine HYDRA
     $(TR,TE,N,NL)
C
C     Rudolf Loeser, 1969 Jul 18
C---- Sets default values of TR.
C     !DASH
      save
C     !DASH
      real*8 TE, TR, ZERO
      integer I, IQ, J, KQ, N, NC, NL, NOION
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
      equivalence (KZQ( 94),NOION)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  MOVE1, SET1, HI, BYE
C
C               TR(N,NL), TE(N)
      dimension TR(N,*),  TE(*)
C     !EJECT
C
      call HI ('HYDRA')
C     !BEG
      if(NOION.le.0) then
C----   Loop over all levels
        do 101 J = 1,NL
C----     Find KQ = # of TR .gt. 0, and IQ = index of first TR .gt. 0
          IQ = 0
          KQ = 0
          do 100 I = 1,N
            if(TR(I,J).gt.ZERO) then
              KQ = KQ+1
              if(IQ.le.0) then
                IQ = I
              end if
            end if
  100     continue
C
          if(KQ.le.0) then
C----       No values .gt. 0
            if(J.eq.1) then
C----         Set = TE
              call MOVE1 (TE     ,N,TR(1,1))
            else
C----         Set = TR[1]
              call MOVE1 (TR(1,1),N,TR(1,J))
            end if
          else if(KQ.lt.N) then
C
C----       If not all values .gt. 0, then set up missing ones
            NC = IQ-1
            if(NC.gt.0) then
              call SET1  (TR(1,J),NC,TR(IQ,J))
            end if
            I  = IQ+KQ
            NC = N-I+1
            if(NC.gt.0) then
              call MOVE1 (TE(I)  ,NC,TR( I,J))
            end if
          end if
  101   continue
      end if
C     !END
      call BYE ('HYDRA')
C
      return
      end
