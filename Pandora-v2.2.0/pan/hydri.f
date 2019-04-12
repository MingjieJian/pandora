      subroutine HYDRI
     $(RABD,RABDL,RZM,BDHM,N,QELSM)
C
C     Rudolf Loeser, 1971 Jun 09
C---- Sets default values for RABD, RZM, and BDHM.
C     !DASH
      save
C     !DASH
      real*8 BDHM, RABD, RABDL, RZM, SIG, TEN, ZERO
      integer I, N, NOION
      logical ZA
      character QELSM*8
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
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external NAUGHTD, ONE1, HI, BYE
C
C               RABD(N), RZM(N), BDHM(N), RABDL(N)
      dimension RABD(*), RZM(*), BDHM(*), RABDL(*)
C
      data SIG /-3.D2/
C
      call HI ('HYDRI')
C     !BEG
      if(NOION.le.0) then
        call NAUGHTD (RABDL, 1, N, ZA)
        if(.not.ZA) then
          do 100 I = 1,N
            if(RABDL(I).le.SIG) then
              RABD(I) = ZERO
            else
              RABD(I) = TEN**RABDL(I)
            end if
  100     continue
        end if
C
        call NAUGHTD (RABD, 1, N, ZA)
        if(ZA.or.(QELSM.eq.'H  ')) then
          call ONE1  (RABD, N)
        end if
      end if
C
      call NAUGHTD   (RZM, 1, N, ZA)
      if(ZA) then
        call ONE1    (RZM, N)
      end if
C
      call NAUGHTD   (BDHM, 1, N, ZA)
      if(ZA) then
        call ONE1    (BDHM, N)
      end if
C     !END
      call BYE ('HYDRI')
C
      return
      end
