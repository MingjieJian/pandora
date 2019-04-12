      subroutine HESSON
     $(N,Z,DEL,CRIT,CALLER)
C
C     Rudolf Loeser, 1997 Aug 28
C---- Computes and checks delta-Z, preceding interval.
C     (See also BEGGAR.)
C     (This is version 2 of HESSON.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, DEL, Z, ZERO
      integer I, KODE, LUEO, N
      character CALLER*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external COMPD, CATRIN, MESHED, DVECOUT, ABORT, HI, BYE
C
C               Z(N), DEL(N)
      dimension Z(*), DEL(*)
C     !EJECT
C
      call HI ('HESSON')
C     !BEG
      DEL(1) = ZERO
C
      do 101 I = 2,N
C
        call COMPD     (Z(I),Z(I-1),CRIT,KODE)
C
        if(KODE.eq.0) then
          call MESHED  ('HESSON',1)
          write (LUEO,100) CALLER
  100     format(' ','Trouble computing delta-Z. Called from ',A)
          call DVECOUT (LUEO,Z,N,'Z')
          call ABORT
        end if
C
        DEL(I) = Z(I)-Z(I-1)
  101 continue
C
      call CATRIN      (DEL,N)
C     !END
      call BYE ('HESSON')
C
      return
      end
