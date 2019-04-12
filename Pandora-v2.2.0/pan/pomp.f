      subroutine POMP
     $(X,W,XPBL,LZA,ZAUX)
C
C     Rudolf Loeser, 1973 Jun 21
C---- Controls the reading of the 4. batch of input statements -
C     populations data.
C     !DASH
      save
C     !DASH
      real*8 W, X, XPBL, ZAUX, dummy
      integer IPNT, JJZ, K, KERR, LLBD, LLPOPK, LLPOPN, LOOK, LUEO, LZA,
     $        MODE, NMISCT, NTOT, jummy
      character GO*8, LMISCT*8, QALL*8, QNAME*8
C     !COM
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
      equivalence (LZOQ( 3),LLPOPK)
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 5),LLBD  )
C     !EJECT
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
C
C---- ZINDEX      as of 1984 Apr 24
      integer     MAUXZI
      common      /ZINDEX/ MAUXZI
C     Auxiliary Z-scale index, for input processing.
C     .
C---- CARGOT      as of 1996 Dec 10
      logical     ESCARGO
      common      /CARGOT/ ESCARGO
C     Input reading signal from subroutine CARMEN.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external KIWI, LOOKUC, BOY, BRIAN, POPIO, DOKIMOS, CARMEN, UNMIX,
     $         CHLOE, ABORT, HI, BYE
C
      dimension X(*), W(*)
C
C               XPBL(Lenpbl), ZAUX(NZM,LZM), LZA(50)
      dimension XPBL(*),      ZAUX(*),       LZA(*)
C
      parameter (NMISCT=7)
      parameter (NTOT=NMISCT+3*NPI)
C
      dimension IPNT(NTOT), QALL(NTOT), LMISCT(NMISCT)
C
      data GO     /'GO'/
      data LMISCT /'MAUX', 'ZAUX', 'USE', 'POPION', 'FILE', 'POPXLM',
     $             'POPRCP'/
C     !EJECT
C
      call HI ('POMP')
C     !BEG
      KERR = 0
C---- Read next input field
  100 continue
        call KIWI    (MODE, dummy, jummy, QNAME, jummy)
        if(MODE.ne.2) goto 205
        call UNMIX   (QNAME)
C----   Check for ionized population data
        call LOOKUC  (KLABPI, NPI, QNAME, K, LOOK)
        if(LOOK.eq.1) then
          call POPIO ('EXCHANGE', K, XPBL)
          call BOY   (1, XPBL(LLPOPK), 1, QNAME, LZA, ZAUX, X(JJZ), W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for level population data
        call LOOKUC  (NLABPI, NPI, QNAME, K, LOOK)
        if(LOOK.eq.1) then
          call POPIO ('EXCHANGE', K, XPBL)
          call BOY   (2, XPBL(LLPOPN), LIMPOP(K), QNAME, LZA, ZAUX,
     $                X(JJZ), W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for departure coefficients
        call LOOKUC  (BLABPI, NPI, QNAME, K, LOOK)
        if(LOOK.eq.1) then
          call POPIO ('EXCHANGE', K, XPBL)
          call BOY   (2, XPBL(LLBD), LIMPOP(K), QNAME, LZA, ZAUX,
     $                X(JJZ), W)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for miscellaneous stuff
        call LOOKUC  (LMISCT, NMISCT, QNAME, K, LOOK)
        if(LOOK.eq.1) then
          call BRIAN (K, QNAME, MAUXZI, LZA, ZAUX)
          if(ESCARGO) goto 199
          goto 100
        end if
C----   Check for end-of-input
        if(QNAME.ne.GO) goto 202
C---- Wrap up, making sure to save Block currently in buffer.
      call POPIO   ('WRITE', jummy, XPBL)
      goto 199
C     !EJECT
C---- Error messages
  205 KERR = KERR+1
  204 KERR = KERR+1
  203 KERR = KERR+1
  202 KERR = KERR+1
  201 KERR = KERR+1
      call DOKIMOS (LMISCT, NMISCT, KLABPI, NPI, NLABPI, NPI, BLABPI,
     $              NPI, QALL, IPNT, LUEO)
      call CHLOE   (LUEO, QNAME, KERR)
      call ABORT
      call CARMEN
      if(ESCARGO) goto 199
      goto 100
C
C---- Go home
  199 continue
C     !END
      call BYE ('POMP')
C
      return
      end
