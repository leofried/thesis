
let two () =
  let iters = 10000000 in
  Analysis.analyze_scheme ~iters ~luck:0.33 (Scheme_round_robin.make 3);
  Analysis.analyze_scheme ~iters ~luck:0.33 (Scheme_simple_bracket.make 3);
  Analysis.analyze_scheme ~iters ~luck:1.00 (Scheme_round_robin.make 3);
  Analysis.analyze_scheme ~iters ~luck:1.00 (Scheme_simple_bracket.make 3);
  Analysis.analyze_scheme ~iters ~luck:3.00 (Scheme_round_robin.make 3);
  Analysis.analyze_scheme ~iters ~luck:3.00 (Scheme_simple_bracket.make 3);

