from itertools import tee

_DEBUG = False


def backtrack(
    solution_space=(),
    is_solution=lambda *args: True,
    generate_candidates=lambda *args: (),
    make_move=None,
    unmake_move=None,
    context=None,
    recursion_limit=3,
):
    if _DEBUG:
        print("start:", solution_space, context)
    if is_solution(solution_space, context):
        if _DEBUG:
            print("---> solution:", solution_space)
        yield solution_space
    else:
        if recursion_limit == 0:
            print("Recursion limit reached")
            return
        candidates, debug_candidates = tee(generate_candidates(solution_space, context))
        if _DEBUG:
            print("candidates:", list(debug_candidates))
        for candidate in candidates:
            if _DEBUG:
                print("trying candidate:", candidate)
            if callable(make_move):
                make_move(solution_space, candidate, context)
                if _DEBUG:
                    print("made move:", context)
            yield from backtrack(
                solution_space=solution_space + (candidate,),
                is_solution=is_solution,
                generate_candidates=generate_candidates,
                make_move=make_move,
                unmake_move=unmake_move,
                context=context,
                recursion_limit=recursion_limit - 1
            )
            if callable(unmake_move):
                unmake_move(solution_space, candidate, context)
                if _DEBUG:
                    print("unmade move:", context)
