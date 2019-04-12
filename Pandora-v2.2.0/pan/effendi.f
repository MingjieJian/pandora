      subroutine EFFENDI
     $(NHITER,TKIN,R,P,ZME,FT,HNDP,HND)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Prints debug H.S.E. data.
C     !DASH
      save
C     !DASH
      real*8 FT, HND, HNDP, P, R, TKIN, ZME
      integer I, LUEO, N, NHITER
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, MESHED, MASHED, HI, BYE
C
C               TKIN(N), R(N), P(N), ZME(N), FT(N), HNDP(N), HND(N)
      dimension TKIN(*), R(*), P(*), ZME(*), FT(*), HNDP(*), HND(*)
C
      call HI ('EFFENDI')
C     !BEG
      call MESHED ('EFFENDI', 2)
      write (LUEO,100) NHITER
  100 format(' ','HSE:  NH-iteration',I3//
     $       ' ',14X,'TKIN',14X,'R',14X,'P',12X,'ZME',14X,'F',11X,
     $           'HNDP',12X,'HND')
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,TKIN(I),R(I),P(I),ZME(I),FT(I),HNDP(I),
     $                HND(I),I=1,N)
  101 format(5(' ',I3,1P7E15.7/))
      call MASHED ('EFFENDI')
C     !END
      call BYE ('EFFENDI')
C
      return
      end
