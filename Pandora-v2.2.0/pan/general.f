      subroutine GENERAL
     $(TAU,IMAX,N,FIN,WN,W,KODE)
C
C     Rudolf Loeser, 1989 Nov 01
C---- Computes WN by the "general ray-tracing" method.
C     Returns with KODE=1 if alll seems OK, with KODE=0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, TMS, W, WN
      integer IMAX, KASE, KODE, N
      logical FIN
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
      equivalence (RZQ( 44),TMS  )
C     !DASH
      external  AMUR, SOTUR, HI, BYE
C
      dimension W(*)
C
C               TAU(N,N), WN(N,N)
      dimension TAU(*),   WN(*)
C
      data KASE /4/
C
      call HI ('GENERAL')
C     !BEG
      call SOTUR (1,KASE,-1,-1)
      call AMUR  (TAU,IMAX,N,FIN,TMS,WN,W)
      KODE = 1
C     !END
      call BYE ('GENERAL')
C
      return
      end
