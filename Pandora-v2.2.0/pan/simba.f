      subroutine SIMBA
     $(LFB,WTAB,N,TMU,SNU,WS,YNT,MUX,YY,KODE)
C
C     Rudolf Loeser, 1980 Jul 08
C---- Computes Emergent Intensity; and
C     the integration weights needed for this calculation, and
C     (when LFB=2) revised values of TMU and SNU.
C
C---- Upon return:
C     WS   contains the weights, of length N;
C     YNT  is the Intensity;
C     MUX  is the index of the maximum term of the summation;
C     YY   is the sum of the fractional contributions from MUX and
C             the two neighboring points;
C
C     KODE .eq. 0 means: the calculation seems OK;
C               1      : there are only .le. 3 terms in the sum, and
C                        no values of WS, YNT, YY, or MUX were computed;
C               2      : warning -- TMU(N) .lt. 2.0;
C               3      : the computed value of YNT was inconsistent,
C                        and was set = 0 (checked only when
C                        option IVALICK is on).
C
C     (This is version 3 of SIMBA.)
C     !DASH
      save
C     !DASH
      real*8 SNU, TMU, WS, WTAB, YNT, YY
      integer IL, IS, ISMBD, KODE, KOUNTI, LFB, M, MUX, N
      logical GO, KILROY
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
      equivalence (KZQ( 92),ISMBD)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  HALT, MAGYAR, WEASEL, HYENA, PUMA, REVERSD, MARTEL,
     $          LION, HI, BYE
      intrinsic mod
C
C               TMU(N), WS(N), SNU(N)
      dimension TMU(*), WS(*), SNU(*)
C
      data KILROY /.true./
C
      call HI ('SIMBA')
C     !BEG
      if(LFB.eq.2) then
C----   "Reverse" TMU and SNU
        call MARTEL   (TMU, N, WTAB)
        call REVERSD  (SNU, 1, N)
      else if(LFB.ne.1) then
        write (MSSLIN(1),100) LFB
  100   format('LFB (frontface/backface code) =',I12,', which is ',
     $         'neither 1 nor 2.')
        call HALT     ('SIMBA', 1)
      end if
C
C---- Compute limiting indices for summation
      call LION       (TMU, N, M, IS, IL)
C
C---- Initialize, and check for basic errors
      call PUMA       (WS, TMU, N, M, YNT, YY, MUX, KODE, GO)
C
      if(GO) then
C----   Compute integration weights
        call WEASEL   (TMU, N, IS, IL, WS)
C----   Compute (and check?) intensity
        call HYENA    (WS, SNU, N, YNT, MUX, YY, KODE)
      end if
C
      if(ISMBD.gt.0) then
C----   Dump administration
        if(KILROY) then
          KILROY = .false.
          KOUNTI = 0
        end if
        KOUNTI = KOUNTI+1
        if(mod(KOUNTI,ISMBD).eq.0) then
          call MAGYAR (WTAB, KOUNTI, ISMBD, N, TMU, SNU, WS, YNT, YY,
     $                 MUX, KODE)
        end if
      end if
C     !END
      call BYE ('SIMBA')
C
      return
      end
