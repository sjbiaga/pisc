import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import scalajs from '@scala-js/vite-plugin-scalajs';

export default defineConfig({
  plugins: [react(), scalajs({ cwd: ".." })],
  server: {
    proxy: {
      '/redpanda-proxy': {
        target: 'http://localhost:18082',
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/redpanda-proxy/, ''),
        configure: (proxy, options) => {
          proxy.on('proxyReq', (proxyReq, req, res) => {
            if (req.method === 'OPTIONS') {
              res.writeHead(200, {
                'Access-Control-Allow-Origin': '*',
                'Access-Control-Allow-Methods': 'GET, POST, PUT, DELETE, OPTIONS',
                'Access-Control-Allow-Headers': 'Content-Type, Accept, Authorization',
              });
              res.end();
            }
            if (req.url.includes('/consumers/')) {
              proxyReq.appendHeader('Accept', 'application/vnd.kafka.v2+json');
              proxyReq.appendHeader('Content-Type', 'application/vnd.kafka.v2+json');
            }
          });
        },
      },
    },
  },
  server: {
    proxy: {
      '/queue': {
        target: 'http://localhost:9324',
        changeOrigin: true
      },
      '/': {
        target: 'http://localhost:9324',
        changeOrigin: true,
        bypass: (req) => {
          if (
            req.headers.accept?.includes('text/html') ||
            req.url.startsWith('/node_modules') ||
            req.url.startsWith('/src') ||
            req.url.startsWith('/@') ||
            req.url.includes('.js?') ||
            req.url.endsWith('.js') ||
            req.url.endsWith('.css')
          ) {
            return req.url;
          }
          return null;
        }
      }
    }
  },
});
