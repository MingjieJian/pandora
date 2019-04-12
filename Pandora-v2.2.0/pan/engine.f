      subroutine ENGINE
     $(XM,W,IW,N,TIT,DUMP,ISIG)
C
C     Rudolf Loeser, 1982 Mar 23
C
C---- Inverts M-matrices for Source Function calculations.
C     The inverse of XM replaces XM.
C
C     If DUMP is true, then XM will be printed to unit LUEO
C     both before and after inversion.
C     TIT is a character variable containing an identifying label for
C     the matrix, to be printed with the dump and/or with any error
C     message from the inversion routine.
C
C     Upon return, ISIG = 1 indicates that inversion appeared to proceed
C     properly; whereas ISIG = 0 indicates that inversion failed and XM
C     now contains junk.
C     !DASH
      save
C     !DASH
      real*8 W, XM
      integer ISIG, IW, LUEO, N
      logical DUMP
      character TIT*80
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ARROUT, MOTOR, HI, BYE
C
      dimension W(*), IW(*)
C
C               XM(N,N)
      dimension XM(*)
C
      call HI ('ENGINE')
C     !BEG
      if(DUMP) then
        call ARROUT (LUEO, XM, N, N, (TIT//' -- Given matrix M'))
      end if
C
      ISIG = 1
      call MOTOR    (XM, N, TIT, W, IW, ISIG)
C
      if((ISIG.gt.0).and.DUMP) then
        call ARROUT (LUEO, XM, N, N, (TIT//' -- Inverse 1/M'))
      end if
C     !END
      call BYE ('ENGINE')
C
      return
      end
