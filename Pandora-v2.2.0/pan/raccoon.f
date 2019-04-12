      subroutine RACCOON
     $(K,DL,NR,PHI,SWITCH)
C
C     Rudolf Loeser, 1993 Feb 03
C---- Enters data into plot, for CANARY.
C     !DASH
      save
C     !DASH
      real*8 DL, P, PHI, ZERO
      integer I, J, K, L, LINC, NR
      logical SWITCH
      character PERIOD*1, PLOT*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
C     !EJECT
      external LINK, KPLOTC, HI, BYE
C
C               DL(KM), PHI(N ,K)
      dimension DL(*),  PHI(NR,*)
C
      call HI ('RACCOON')
C     !BEG
      do 102 L = 1,2
C
        do 101 I = 1,NR
C
          PLOT = ALPHS(I)
          if(SWITCH.and.(L.eq.1)) then
            PLOT = PERIOD
          end if
C
          LINC = 1
          do 100 J = 1,K
            if(PHI(I,J).le.ZERO) then
              LINC = 1
            else
              P = log10(PHI(I,J))
              if(L.eq.2) then
                call KPLOTC (IMAGE, DL(J), P, PLOT)
              else
                call LINK   (IMAGE, DL(J), P, PLOT, LINC)
              end if
            end if
  100     continue
C
  101   continue
        if(.not.SWITCH) goto 103
C
  102 continue
  103 continue
C     !END
      call BYE ('RACCOON')
C
      return
      end
