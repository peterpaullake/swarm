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
#include <cstdlib>
#include <vector>

#ifdef USE_ODEINT
using namespace boost::numeric::odeint;
#endif

typedef std::vector<double> state_type;
// typedef double* state_type;

const int n = 300;
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
  int m_i;
  const int m_state_size;
  double* m_buffer;

  push_back_state_and_time(const int state_size,
			   double* buffer)
    : m_i(0), m_state_size(state_size), m_buffer(buffer) {}

  void operator()(const state_type &state , const double /*t*/) {
    /* If this is not the first step (which is
       just the initial conditions), save it */
    if (m_i > 0) {
      int i = m_i - 1;
      memcpy(&m_buffer[i * m_state_size],
	     &state[0],
	     sizeof(double) * m_state_size);
    }
    m_i++;
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

double* allocate_state_buffer(const int state_size, const int n_states) {
  return (double*)malloc(sizeof(double) * state_size * n_states);
}

void free_state_buffer(double* buffer) {
  free(buffer);
}

void set_initial_conditions(const int state_size, double* buffer) {
  /* Initialize the predator and prey positions
     randomly inside the unit square [0,1] x [0,1] */
  for (int i = 0; i < state_size; i++) {
    buffer[i] = (double)rand() / (double)RAND_MAX;
  }
}

void copy_initial_conditions(const int state_size,
			     const int n_states,
			     const double* buffer_src,
			     double* buffer_dest) {
  /* Copy the last state of the given states buffer to another state buffer */
  memcpy(buffer_dest,
	 &buffer_src[state_size * (n_states - 1)],
	 sizeof(double) * state_size);
}

void simulate(const int state_size, const int n_states,
	      const double* buffer0, double* buffer) {
  adams_bashforth_moulton<5, state_type> stepper;
  state_type state0 = state_type(buffer0, buffer0 + state_size);
  integrate_n_steps(stepper, rhs, state0, 0.0, 0.1, n_states,
		    push_back_state_and_time(state_size, buffer));
}

#ifndef USE_WASM
int main() {
  const int state_size = 2 * (n + 1);
  double* buffer0 = allocate_state_buffer(state_size, 1);
  set_initial_conditions(state_size, buffer0);

  const int n_states = 60;
  double* buffer = allocate_state_buffer(state_size, n_states);

  simulate(state_size, n_states, buffer0, buffer);

  free_state_buffer(buffer);
  free_state_buffer(buffer0);
  return 0;
}
#endif