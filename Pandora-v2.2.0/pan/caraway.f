      subroutine CARAWAY
     $(YNT,NF,N,FREQ,WAVE,SUM,TIT,DUMP)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Computes integrals over frequency, from an array
C     of functions of frequency and depth;
C     dumps successive cumulative integrals, if needed.
C     (This is version 3 of CARAWAY.)
C     !DASH
      save
C     !DASH
      real*8 D, FREQ, HALF, SUM, WAVE, YNT
      integer I, J, LUEO, N, NF
      logical DUMP
      character TIT*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ZERO1, VECOUT, LINER, MESHED, MASHED, HI, BYE
C
C               YNT(N,NF), FREQ(NF), WAVE(NF), SUM(N)
      dimension YNT(N,*),  FREQ(*),  WAVE(*),  SUM(*)
C     !EJECT
C
      call HI ('CARAWAY')
C     !BEG
C
      call ZERO1      (SUM, N)
C
      if(DUMP) then
        call MESHED   ('CARAWAY', 2)
        write (LUEO,100) TIT
  100   format(' ','Dump of cumulative ',A,' integral, for each ',
     $             'frequency.')
        call LINER    (1, LUEO)
        write (LUEO,101) FREQ(1),WAVE(1)
  101   format(' ','Frequency =',1PE22.14,10X,'Wavelength =',E22.14)
        call VECOUT   (LUEO, SUM, N, 'Sum')
      end if
C
C
      do 103 J = 2,NF
C
        do 102 I = 1,N
          D = HALF*(FREQ(J-1)-FREQ(J))*(YNT(I,J-1)+YNT(I,J))
          SUM(I) = SUM(I)+D
  102   continue
C
C
        if(DUMP) then
          call LINER  (1, LUEO)
          write (LUEO,101) FREQ(J),WAVE(J)
          call VECOUT (LUEO, SUM, N, 'Sum')
        end if
C
  103 continue
C
      if(DUMP) then
        call MASHED   ('CARAWAY')
      end if
C     !END
      call BYE ('CARAWAY')
C
      return
      end
