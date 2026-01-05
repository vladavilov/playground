import { useToastStore } from "../../state/toast";
import { Toast, ToastClose, ToastDescription, ToastProvider, ToastTitle, ToastViewport } from "./toast";

export function Toaster() {
  const toasts = useToastStore((s) => s.toasts);
  const dismiss = useToastStore((s) => s.dismiss);

  return (
    <ToastProvider swipeDirection="right">
      {toasts.map((t) => (
        <Toast
          key={t.id}
          open
          onOpenChange={(open) => {
            if (!open) dismiss(t.id);
          }}
          duration={4000}
          variant={t.variant}
        >
          <div className="grid gap-1">
            <ToastTitle>{t.title}</ToastTitle>
            {t.description ? <ToastDescription>{t.description}</ToastDescription> : null}
          </div>
          <ToastClose />
        </Toast>
      ))}
      <ToastViewport className="fixed bottom-0 right-0 z-50 m-4 flex w-96 max-w-[calc(100vw-2rem)] flex-col gap-2 outline-none" />
    </ToastProvider>
  );
}



