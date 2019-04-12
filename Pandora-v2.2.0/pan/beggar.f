      subroutine BEGGAR
     $(N,Z,DEL,CRIT,CALLER)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Computes and checks delta-Z, bridging intervals.
C     (SEe also HESSON.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, DEL, Z
      integer I, IA, IB, KODE, LUEO, N
      character CALLER*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external COMPD, MESHED, DVECOUT, CATRIN, ABORT, HI, BYE
C
C               Z(N), DEL(N)
      dimension Z(*), DEL(*)
C     !EJECT
C
      call HI ('BEGGAR')
C     !BEG
      do 101 I = 1,N
        IA = I-1
        IB = I+1
        if(I.eq.1) then
          IA = 1
        else if(I.eq.N) then
          IB = N
        end if
C
        call COMPD     (Z(IB),Z(IA),CRIT,KODE)
C
        if(KODE.eq.0) then
          call MESHED  ('BEGGAR',1)
          write (LUEO,100) CALLER
  100     format(' ','Trouble computing delta-Z. Called from ',A)
          call DVECOUT (LUEO,Z,N,'Z')
          call ABORT
        end if
C
        DEL(I) = Z(IB)-Z(IA)
  101 continue
C
      call CATRIN      (DEL,N)
C     !END
      call BYE ('BEGGAR')
C
      return
      end
