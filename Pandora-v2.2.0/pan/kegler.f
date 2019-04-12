      subroutine KEGLER
     $(N,T,ANT,UNT,CNDT,PD,FNDT,OMD,DEL)
C
C     Rudolf Loeser, 2004 May 07
C---- Dumps for RODA.
C     (This is version 2 of KEGLER.)
C     !DASH
      save
C     !DASH
      real*8 ANT, CNDT, DEL, FNDT, OMD, PD, T, UNT
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               CNDT(N), FNDT(N), ANT(N), UNT(N), OMD(N), DEL(N), T(N),
      dimension CNDT(*), FNDT(*), ANT(*), UNT(*), OMD(*), DEL(*), T(*),
C
C               PD(N)
     $          PD(*)
C
      call HI ('KEGLER')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100)
  100 format(' ',19X,'T',12X,'ANT',12X,'UNT',11X,'CNDT',13X,'PD',
     $           11X,'FNDT',12X,'OMD',12X,'DEL')
      call LINER (1, LUEO)
      write (LUEO,101) (I,T(I),ANT(I),UNT(I),CNDT(I),PD(I),FNDT(I),
     $                  OMD(I),DEL(I),I=1,N)
  101 format(5(' ',I5,1P8E15.7/))
C     !END
      call BYE ('KEGLER')
C
      return
      end
