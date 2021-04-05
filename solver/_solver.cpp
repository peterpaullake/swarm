#define USE_ODEINT
// #define USE_WASM

#ifdef USE_ODEINT
#include <boost/numeric/odeint.hpp>
#endif

#ifdef USE_WASM
#include <emscripten.h>
#else
#include <iostream>
#endif

#include <cmath>
#include <stdlib.h>
#include <vector>

typedef std::vector<double> state_type;

const int n = 400;
const double a = 0.796;
const double b = 0.708;
const double c = 15.0;
const double p = 2.1985;

double len2(double x, double y, double xp, double yp) {
  /* Return the squared Euclidean distance
     between the vectors (x, y) and (xp, yp) */
  return pow((x - xp), 2) + pow((y - yp), 2);
}

void compute_pred_dxdt(const double pred_x,
		       const double pred_y,
		       const state_type &state,
		       state_type &dxdt) {
  /* Zero out the derivatives for the predator */
  dxdt[0] = 0.0;
  dxdt[1] = 0.0;
  /* Add the prey influences */
  for (int prey_id = 0; prey_id < n; prey_id++) {
    const int id_x = 2 + 2 * prey_id;
    const int id_y = id_x + 1;
    const double x = state[id_x];
    const double y = state[id_y];
    const double delta_x = x - pred_x;
    const double delta_y = y - pred_y;
    const double lp = pow(len2(x, y, pred_x, pred_y), p / 2.0);
    dxdt[0] += c / n * delta_x / lp;
    dxdt[1] += c / n * delta_y / lp;
  }
}

void compute_prey_dxdt(const int prey_id,
		       const double pred_x,
		       const double pred_y,
		       const state_type &state,
		       state_type &dxdt) {
  /* Extract the coordinates of the current prey from the state vector */
  const int id_x = 2 + 2 * prey_id;
  const int id_y = id_x + 1;
  const double x = state[id_x];
  const double y = state[id_y];
  /* Zero out the derivatives for the current prey */
  dxdt[id_x] = 0.0;
  dxdt[id_y] = 0.0;
  /* Add the prey influences */
  for (int prey_idp = 0; prey_idp < n; prey_idp++) {
    if (prey_idp != prey_id) {
      const int id_xp = 2 + 2 * prey_idp;
      const int id_yp = id_xp + 1;
      const double xp = state[id_xp];
      const double yp = state[id_yp];
      const double delta_x = x - xp;
      const double delta_y = y - yp;
      const double l2 = len2(x, y, xp, yp);
      dxdt[id_x] += (delta_x / l2 - a * delta_x) / n;
      dxdt[id_y] += (delta_y / l2 - a * delta_y) / n;
    }
  }
  /* Add the predator influence */
  const double delta_x = x - pred_x;
  const double delta_y = y - pred_y;
  const double l2 = len2(x, y, pred_x, pred_y);
  dxdt[id_x] += b * delta_x / l2;
  dxdt[id_y] += b * delta_y / l2;
}

void rhs(const state_type &state, state_type &dxdt, const double) {
  /* Extract the predator coordinates */
  const double pred_x = state[0];
  const double pred_y = state[1];
  /* Compute the predator derivatives */
  compute_pred_dxdt(pred_x, pred_y, state, dxdt);
  /* Compute the prey derivatives */
  for (int prey_id = 0; prey_id < n; prey_id++) {
    compute_prey_dxdt(prey_id, pred_x, pred_y, state, dxdt);
  }
}

#ifndef USE_WASM
void print_state(const state_type &state, const double t) {
  std::cout << "t = " << t << ", ";
  std::cout << "x = (" << state[0] << ",\t" << state[1] << ")\n";
}
#endif

struct push_back_state_and_time {
  std::vector<state_type>& m_states;
  std::vector<double>& m_times;

  push_back_state_and_time(std::vector<state_type> &states,
			   std::vector<double> &times)
    : m_states(states), m_times(times) {}

  void operator()(const state_type &state , const double t) {
    m_states.push_back(state);
    m_times.push_back(t);
  }
};

#ifdef USE_WASM
/* We do extern "C" here so that the name of our function is
   not mangled, which would prevent emscripten from finding it */
extern "C" {
  EMSCRIPTEN_KEEPALIVE
  double version() {
    return 23.4;
  }
}
#endif

#ifndef USE_WASM
int main() {
#ifdef USE_ODEINT
  using namespace boost::numeric::odeint;
#endif

  /* Create the initial conditions */
  const int state_size = 2 + 2 * n;
  state_type state0 = state_type(state_size);
  for (int i = 0; i < state_size; i++) {
    state0[i] = (double)rand() / (double)RAND_MAX;
  }

  /* Create the vectors to hold the solution */
  std::vector<state_type> states;
  std::vector<double> times;

#ifdef USE_ODEINT
  /* Use a multistep method to solve the equation. We use a
     multistep method because calling our right-hand side
     function is expensive (nested for loops), and multistep
     methods are supposed to limit the amount of calls. */
  adams_bashforth_moulton<5, state_type> stepper;
  // adams_bashforth<5, state_type> stepper;
  integrate_n_steps(stepper, rhs, state0, 0.0, 0.01, 120,
		    push_back_state_and_time(states, times));
  for (size_t i = 0; i < states.size(); i++) {
    print_state(states[i], times[i]);
    if (i > 0) {
      std::cout << "Stepped by " << times[i] - times[i - 1] << "\n";
    }
  }
  std::cout << "states.size() = " << states.size() << ", ";
  std::cout << "times.size() = " << times.size() << "\n";
#endif

  return 0;
}
#endif

/*
  Initialize ics randomly
  Initialize the ics from javascript
  Find out how to change the rhs params after compile time
*/
