import { registerHooks } from 'node:module';

registerHooks({
  resolve(specifier, context, nextResolve) {
    // If an ESM import tries to read a .css file, return a virtual Javascript URI
    if (specifier.endsWith('.css')) {
      return {
        shortCircuit: true,
        url: 'data:text/javascript,export default {};'
      };
    }
    // Otherwise, defer to Node's default resolution algorithm
    return nextResolve(specifier, context);
  }
});
