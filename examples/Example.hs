import CableCar  -- Requires the cablecar directory (outer one, with CableCar.hs) to be in the search path, e.g. by using a "-i" argument.

main :: IO ()
main = putStrLn $ printFL $ makeHardware example

{-
    This example connects a CPU, garbage collector, memory arbiter, and memory.
    The CPU can also use an external cryptographic accelerator.
-}
example :: Plan
example = runCableCar $ do
    begin "example"
    let t_memread   = cabletype nopower handshake ["x" `typed` "addr"] ["dout" `typed` "word"]
    let t_memwrite  = cabletype nopower handshake ["x" `typed` "addr", "din" `typed` "word"] []
    let t_interrupt = cabletype nopower nohandshake ["flag" `typed` "bit"] []
    let t_crypt     = cabletype nopower handshake ["key" `typed` "key", "x" `typed` "block"] ["y" `typed` "block"]
    -- cables
    memread      <- cable "memread"      t_memread
    memwrite     <- cable "memwrite"     t_memwrite
    memread_cpu  <- cable "memread_cpu"  t_memread
    memwrite_cpu <- cable "memwrite_cpu" t_memwrite
    memread_gc   <- cable "memread_gc"   t_memread
    memwrite_gc  <- cable "memwrite_gc"  t_memwrite
    interrupt    <- cable "interrupt"    t_interrupt
    encrypt      <- cable "encrypt"      t_crypt
    decrypt      <- cable "decrypt"      t_crypt
    -- i/o for our module
    plug interrupt from world
    plug encrypt into world
    plug decrypt into world
    -- memory module
    mem <- make "memory" []
    plug memread into mem
    plug memwrite into mem
    -- memory arbiter
    arb <- make "memory_arbiter" ["2"]
    plug memread       from arb
    plug memwrite      from arb
    plug memread_cpu   into arb
    plug memwrite_cpu  into arb
    plug memread_gc    into arb
    plug memwrite_gc   into arb
    -- cpu
    cpu <- make "cpu" []
    plug interrupt into cpu
    plug memread_cpu from cpu
    plug memwrite_cpu from cpu
    plug encrypt from cpu
    plug decrypt from cpu
    -- garbage collector
    gc <- make "gc" []
    plug memread_gc from gc
    plug memwrite_gc from gc
