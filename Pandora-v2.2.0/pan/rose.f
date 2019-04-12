      subroutine ROSE
     $(IPOP,H,LIMD,XLM,TE,B,BD,LEVEL,N,SRES,SREM)
C
C     Rudolf Loeser, 1978 Oct 06
C---- Computes a set of population ion bound-free
C     continuum source function terms.
C     (This is version 3 of ROSE.)
C     !DASH
      save
C     !DASH
      real*8 B, BD, H, HNUKT, RAT, SD1, SDR, SEF, SN1, SNR, SREM, SRES,
     $       TE, TN, XD, XLM, XN, ZERO
      integer I, IPOP, J, LEVEL, LIMD, N
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- IONDATA     as of 2007 Jan 12
C     Data tables for the built-in models of "non-LTE" ions; these
C     models are used to compute bound-free absorption and emission.
C     Hydrogen (of course!) is treated as a special case.
      integer     NPION,NIONL,NDPNT
C
      parameter   (NPION=14)
C     NPION     = number of ion models, as follows:
C     01: H      02: C-I    03: Si-I   04: He-I   05: He-II  06: Al-I
C     07: Mg-I   08: Fe-I   09: Na-I   10: Ca-I   11: O-I    12: S-I
C     13: O-II   14: O-III
C
      parameter   (NIONL=15)
C     NIONL     = maximum number of levels in each model
C
      parameter   (NDPNT=150)
C     NDPNT     = maximum length of tables specifying the wavelength-
C     dependence of the absorption in each continuum, by
C     piece-wise linear approximation.
C
C     REMEMBER to recompile all users when changing NPION, NIONL, NDPNT.
C
      real*8      PILEVL,XLMTHR,CCPLEV,SCPLEV,XLMTAB,RCPTAB
      integer     LIMDAT,MAXDATL,LEND,NPTABL
      character   LLABEL*16
      logical     LLPRNT
      dimension   LIMDAT(            NPION), LLPRNT(            NPION),
     $            PILEVL(      NIONL,NPION), XLMTHR(      NIONL,NPION),
     $            NPTABL(      NIONL,NPION), CCPLEV(      NIONL,NPION),
     $            SCPLEV(      NIONL,NPION), LLABEL(      NIONL,NPION),
     $            XLMTAB(NDPNT,NIONL,NPION), RCPTAB(NDPNT,NIONL,NPION)
C
C     LIMDAT    = actual number of levels in each model (LIMDAT should
C                 equal LIMPOP in labelled common POPDATA)
C     MAXDATL   = maximum value of LIMDAT
C     LEND      = sum of LIMDAT
C     LLABEL    = "term designation" of each level of each model
C     LLPRNT    = data tables print switch
C     PILEVL    = statistical weight of each level of each model
C     XLMTHR    = threshhold wavelengths of continua
C     NPTABL    = actual number of data points in absorption data table
C                 of each level of each model (used only when > 0)
C     CCPLEV    = threshhold absorption factors
C     SCPLEV    = exponent of power-law wavelength dependence of
C                 absorption of each level of each model (used only
C                 when > 0; should be > 0 when corresponding NPTABL = 0)
C     XLMTAB    = wavelength values for which RCPTAB is specified
C     RCPTAB    = absorption in the continuum of a level of a model
C                 (At wavelengths < XLMTAB(NPTABL), a power law
C                  with exponent = 3 is used.)
C
      common      /IODAT01/ MAXDATL,LEND
      common      /IODAT02/ LIMDAT
      common      /IODAT03/ LLPRNT
      common      /IODAT04/ PILEVL
      common      /IODAT05/ XLMTHR
      common      /IODAT06/ NPTABL
      common      /IODAT07/ CCPLEV
      common      /IODAT08/ SCPLEV
      common      /IODAT09/ LLABEL
      common      /IODAT10/ XLMTAB
      common      /IODAT11/ RCPTAB
C     .
C     !DASH
C     !EJECT
      external THORN, DIVIDE, BOBBIE, VETIVER, QEXP1, PROD, MASHED,
     $         HI, BYE
C
C               H(LIMPOP), BD(N,LIMPOP), TE(N), B(N), SRES(N), SREM(N)
      dimension H(*),      BD(N,*),      TE(*), B(*), SRES(*), SREM(*)
C
      call HI ('ROSE')
C     !BEG
      do 101 I = 1,N
        call VETIVER    (I, IPOP, XLM, DUMP, 'ROSE')
C
        call PROD       (TE(I), XLM, 2, HNUKT, TN)
        call QEXP1      (HNUKT, TN, 1, SEF)
        SN1 = ZERO
        SD1 = ZERO
        SNR = ZERO
        SDR = ZERO
C
        do 100 J = 1,LIMD
          if(H(J).ne.ZERO) then
            call THORN  (TE(I), XLMTHR(J,IPOP), PILEVL(J,IPOP), H(J),
     $                   BD(I,J), TN, SEF, XN, XD)
          else
            XN = ZERO
            XD = ZERO
          end if
          if(DUMP) then
            call BOBBIE (J, TE(I), HNUKT, TN, BD(I,J), XLMTHR(J,IPOP),
     $                   PILEVL(J,IPOP), H(J), XN, XD)
          end if
          if(J.eq.LEVEL) then
            SN1 = XN
            SD1 = XD
          else
            SNR = SNR+XN
            SDR = SDR+XD
          end if
  100   continue
C
        call DIVIDE     (SN1, SD1, RAT)
        SRES(I) = B(I)*RAT
        call DIVIDE     (SNR, SDR, RAT)
        SREM(I) = B(I)*RAT
  101 continue
      if(DUMP) then
        call MASHED     ('ROSE')
      end if
C     !END
      call BYE ('ROSE')
C
      return
      end
