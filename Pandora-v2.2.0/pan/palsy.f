      subroutine PALSY
     $(LU,N,GMASS,TE,V,VR,VM,PGS,XNE)
C
C     Rudolf Loeser, 1999 Aug 05
C---- Model data, for Kurucz.
C     !DASH
      save
C     !DASH
      real*8 CMPKM, GMASS, PGS, TE, V, VB, VF, VM, VR, XNE, ZERO
      integer I, IQVSW, LU, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(173),IQVSW)
C     !DASH
C     !EJECT
      external YARDEN, HI, BYE
C
C               GMASS(N), TE(N), V(N), VR(M), VM(N), PGS(N), XNE(N)
      dimension GMASS(*), TE(*), V(*), VR(*), VM(*), PGS(*), XNE(*)
C
      call HI ('PALSY')
C     !BEG
      call YARDEN (LU,1,'KURUCZ - MODEL')
C
      write (LU,100) N
  100 format(I10)
      write (LU,101)
  101 format(6X,'mass',10X,'Te',10X,'p(gas)',10X,'Ne',19X,'v(broad)',
     $       6X,'v(flow)')
C
      do 103 I = 1,N
        if(IQVSW.gt.0) then
          VB = VR(I)*CMPKM
        else
          VB = V(I)*CMPKM
        end if
        VF = -VM(I)*CMPKM
        write (LU,102) GMASS(I),TE(I),PGS(I),XNE(I),ZERO,ZERO,VB,VF,I
  102   format(1PE13.6,0PF12.1,1P2E14.6,0P2F5.1,1P2E14.6,I7)
  103 continue
C
      call YARDEN (LU,2,'KURUCZ - MODEL')
C     !END
      call BYE ('PALSY')
C
      return
      end
