      subroutine DOMINO
     $(IU,IL,TE,AIJ,HYDR,MESS,XNU,XNUC,P,LRQ,NPQ,FEQ,DMX,DMN,CE,
     $ NSUB,GOOD,SROK)
C
C     Rudolf Loeser, 1992 Jan 03
C---- Computes CE, the collisional excitation coefficient, for
C     transitions from level IU to level IL, IU > IL,
C     for a neutral atom, by the impact-parameter method
C     according to
C
C     M .   J .   S e a t o n ,
C
C     "The impact parameter method for electron excitation of
C     optically allowed atomic transitions,"
C     Proc.Phys.Soc., 79, 1105 (1962).
C
C---- Input parameters --
C
C     IU   : index of upper level of this transition
C     IL   : index of lower level of this transition
C     AIJ  : Einstein A coefficient for this transition
C     XNU  : level frequencies
C     XNUC : continuum frequencies
C     P    : statistical weights
C     LRQ  : quasi-"rotational quantum numbers L"
C     NPQ  : rotational quantum numbers N
C     TE   : electron temperature
C     HYDR : is "true" if this is Hydrogen, is "false" if not
C     MESS : is "true" if error message is desired
C
C     FEQ, DMX, DMN: control parameters for integration routine
C                    ADAIR (q.v.)
C
C---- Output
C
C     SROK : is false if method failed; otherwise
C     CE   : collisional excitation parameter
C
C     NSUB, GOOD: status information from ADAIR (q.v.)
C
C     (This is version 2 of DOMINO.)
C     !DASH
      save
C     !DASH
      real*8 AIJ, CE, CON4, CONK, DMN, DMX, FEQ, P, PD, RT, RT32, SUM,
     $       TE, TEN, XKIJ, XNU, XNUC, ZERO, dummy
      integer IL, IU, KMAX, LRQ, LU, LUEO, NPQ, NSUB
      logical DUMP, GOOD, HYDR, MESS, SROK
C     !DASH
C     !EJECT
C---- OLIVIA      as of 2006 Mar 08
      real*8      XNUKH,RBAR,DNU,PT,R,TERJ
      integer     IUCE,ILCE
      common      /OLIVIA/ XNUKH,RBAR,DNU,PT,R
      common      /OLIVIB/ TERJ
      common      /OLIVIC/ IUCE,ILCE
C     Parameters for ERIKA: calculation of Collisional Ionization
C     Integral, for the impact-parameter method.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external DIVIDE, EBOLI, ERIKA, ADAIR, MESHED, MASHED, RIGEL,
     $         HI, BYE
C
C               XNUC(NSL), XNU(NSL), LRQ(NSL), NPQ(NSL), P(NSL)
      dimension XNUC(*),   XNU(*),   LRQ(*),   NPQ(*),   P(*)
C
      parameter (KMAX=100)
      dimension PD(6*KMAX)
C
      data CONK,DUMP /1.476D-8, .false./
C
      call HI ('DOMINO')
C     !BEG
C---- Initialize values in common block
      IUCE = IU
      ILCE = IL
      TERJ = TE
      call RIGEL      (64, XNUKH)
      call RIGEL      ( 4, CON4)
      DNU = XNU(IU)-XNU(IL)
      call DIVIDE     ((P(IU)*AIJ), (P(IL)*DNU), R)
      call DIVIDE     (CON4, TE, PT)
      call EBOLI      (XNUC(IL), XNU(IL), LRQ(IL), NPQ(IL), HYDR, MESS,
     $                 SROK)
      if(SROK) then
C
C----   Compute integral: SUM
        LU = 0
        if(DUMP) then
          LU = LUEO
          call MESHED ('ADAIR', 2)
        end if
        call ADAIR    (DNU, (DNU+TEN/PT), ERIKA, dummy, SUM, FEQ, DMX,
     $                 DMN, LU, PD, KMAX, NSUB, GOOD)
        if(DUMP) then
          call MASHED ('ADAIR')
        end if
C
C----   Finish up
        RT   = sqrt(TE)
        RT32 = RT**3
        call DIVIDE   ((CONK*R), (RT32*(DNU**2)), XKIJ)
        CE = XKIJ*SUM
C
      else
        CE = ZERO
      end if
C     !END
      call BYE ('DOMINO')
C
      return
      end
