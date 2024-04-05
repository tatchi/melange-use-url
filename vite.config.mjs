import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { nodeResolve } from '@rollup/plugin-node-resolve';
import rollupReplace from '@rollup/plugin-replace';

// https://vitejs.dev/config/
export default defineConfig({
	publicDir: './public',
	build: {
		outDir: './dist',
	},
	server: {
		watch: {
			ignored: ['**/_opam'],
		},
	},
	plugins: [
		// rollupReplace({
		// 	preventAssignment: true,
		// 	values: {
		// 		__DEV__: JSON.stringify(true),
		// 		'process.env.NODE_ENV': JSON.stringify('development'),
		// 	},
		// }),
		nodeResolve(),
		react(),
	],
});
