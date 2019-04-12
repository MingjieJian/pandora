      subroutine ABSALON
     $(N,NTE,IRFNC,TER,TE)
C
C     Rudolf Loeser, 1994 May 05
C---- Computes the default value of IRFNC,
C     the XNE-reference-index for sample CE and CI values of Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 DIFL, DIFR, ONE, TE, TER
      integer I, IMAX, IMIN, IRFNC, J, N, NTE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  MINMAXD, HI, BYE
      intrinsic max, abs, sign
C
C               TER(NTE), TE(N)
      dimension TER(*),   TE(*)
C     !EJECT
C
      call HI ('ABSALON')
C     !BEG
      IRFNC = max((N/2),1)
C
      if(NTE.gt.0) then
        if(NTE.eq.1) then
          J = 1
        else
          J = NTE/2
        end if
C
        call MINMAXD (TE,1,N,IMIN,IMAX)
        if(TE(IMIN).ge.TER(J)) then
          IRFNC = IMIN
        else if(TE(IMAX).le.TER(J)) then
          IRFNC = IMAX
        else
C
          do 100 I = 2,N
            DIFL = TER(J)-TE(I-1)
            DIFR = TER(J)-TE(I)
            if(sign(ONE,DIFL).ne.sign(ONE,DIFR)) then
              if(abs(DIFL).lt.abs(DIFR)) then
                IRFNC = I-1
              else
                IRFNC = I
              end if
              goto 101
            end if
  100     continue
C
  101     continue
        end if
C
      end if
C     !END
      call BYE ('ABSALON')
C
      return
      end
