      subroutine HUGRY
     $(DOFMV,FMV,HND,Z,KZXST,N,CDZ,CVZ,FMVLM,MFMV)
C
C     Rudolf Loeser, 1993 Jan 28
C---- Computes FMV, fluid velocity multiplier, and
C     establishes default value of CVZ (if necessary).
C     !DASH
      save
C     !DASH
      real*8 CDZ, CVZ, EM, EP, FACTOR, FMV, FMVLM, HALF, HND, ONE, TANH,
     $       X, Z, ZERO
      integer I, KZXST, LUEO, MFMV, N
      logical DOFMV
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- EXPLIM      as of 1986 Apr 14
      real*8      EXPLIM
      common      /EXPLIM/ EXPLIM
C     Maximum argument for G-floating exponential function
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ONE1, EMERY, MESHED, MASHED, HI, BYE
C
C               FMV(N), HND(N), Z(N)
      dimension FMV(*), HND(*), Z(*)
C
      call HI ('HUGRY')
C     !BEG
      if(DOFMV.and.(CDZ.gt.ZERO).and.(KZXST.gt.0)) then
        MFMV = 1
        if(CVZ.eq.ZZLARGE) then
          call EMERY  (HND, Z, N, CVZ)
        end if
C
        do 100 I = 1,N
          X = (Z(I)-CVZ)/CDZ
          if((+X).gt.EXPLIM) then
            TANH = +ONE
          else if((-X).gt.EXPLIM) then
            TANH = -ONE
          else
            EP = exp(+X)
            EM = exp(-X)
            TANH = (EP-EM)/(EP+EM)
          end if
C
          FACTOR = HALF*(ONE-TANH)
          if(FACTOR.lt.FMVLM) then
            FMV(I) = ZERO
          else
            FMV(I) = FACTOR
          end if
  100   continue
C
      else
        MFMV = 0
        call ONE1     (FMV, N)
        if(KZXST.eq.0) then
          call MESHED ('HUGRY', 3)
          write (LUEO,101)
  101     format(' ','Fluid velocity multiplier cannot be computed ',
     $               'because no input Z-table is given.')
          call MASHED ('HUGRY')
        end if
      end if
C     !END
      call BYE ('HUGRY')
C
      return
      end
