import { create } from "zustand";

export type ToastVariant = "default" | "destructive";

export type ToastItem = {
  id: string;
  title: string;
  description?: string;
  variant?: ToastVariant;
};

type ToastStore = {
  toasts: ToastItem[];
  show: (t: Omit<ToastItem, "id"> & { id?: string }) => void;
  dismiss: (id: string) => void;
  clear: () => void;
};

export const useToastStore = create<ToastStore>((set) => ({
  toasts: [],
  show: (t) =>
    set((st) => ({
      toasts: [
        ...st.toasts,
        { ...t, id: t.id ?? `${Date.now()}-${Math.random().toString(16).slice(2)}` },
      ],
    })),
  dismiss: (id) => set((st) => ({ toasts: st.toasts.filter((x) => x.id !== id) })),
  clear: () => set({ toasts: [] }),
}));



