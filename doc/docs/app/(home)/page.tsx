import Link from 'next/link';

export default function HomePage() {
  return (
    <main className="flex flex-1 flex-col justify-center text-center">
      <div className='flex flex-col items-center'>
        <h1 className="mb-4 text-2xl font-bold">Welcome to the Narxia programming language.</h1>
        <p className="text-fd-muted-foreground">
          Go to the{' '}
          <Link
            href="/docs/user/user-intro"
            className="text-fd-foreground font-semibold underline"
          >
            User documentation
          </Link>.
        </p>
      </div>
    </main>
  );
}
