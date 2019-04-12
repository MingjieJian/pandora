      subroutine BUR
     $(IU,IL,NL,NSL,M,ARHO,XND,NOK,VLS,VL)
C
C     Rudolf Loeser, 1978 Jun 27
C---- Provides for supplementary levels, for HAMMER.
C     See also RICE!
C     !DASH
      save
C     !DASH
      real*8 ARHO, EPCBR, PS1, SUM, VL, VLS, XND, ZERO
      integer IL, IU, J, M, NL, NSL
      logical NOK
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
      equivalence (RZQ( 62),EPCBR)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, HI, BYE
C
C               ARHO(NL,NL), XND(NL)
      dimension ARHO(*),     XND(*)
C     !EJECT
C
      call HI ('BUR')
C     !BEG
      VLS = ZERO
C
      if((NSL.gt.NL).and.NOK) then
        if(IL.eq.M) then
          SUM = ZERO
C
          do 100 J = 1,NL
            if(J.ne.M) then
              SUM = SUM+XND(J)*ARHO(J)
            end if
  100     continue
C
          PS1 = EPCBR*SUM
          call DIVIDE (PS1, XND(M), VLS)
          VL = VL+VLS
        end if
      end if
C     !END
      call BYE ('BUR')
C
      return
      end
