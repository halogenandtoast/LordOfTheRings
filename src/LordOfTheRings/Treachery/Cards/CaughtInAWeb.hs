module LordOfTheRings.Treachery.Cards.CaughtInAWeb (caughtInAWeb) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaughtInAWeb = CaughtInAWeb TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

caughtInAWeb :: TreacheryCard CaughtInAWeb
caughtInAWeb = treachery CaughtInAWeb Cards.caughtInAWeb

-- | Counts as a Condition attachment with the text: "Attached hero does not ready during
-- the refresh phase unless you pay 2 resources from that hero's pool."
instance HasModifiersFor CaughtInAWeb where
  getModifiersFor (CaughtInAWeb a) = case a.placement of
    AttachedTo host -> modified a host [DoesNotReadyDuringRefresh]
    _ -> pure ()

instance HasAbilities CaughtInAWeb where
  getAbilities (CaughtInAWeb a) = case a.placement of
    AttachedTo (HeroTarget hid) ->
      [ restricted a 1 (HeroExists $ HeroWithId hid <> ExhaustedHero)
          $ ResponseAbility (PhaseBeginsWindow After RefreshPhase) (ResourceCost (HeroTarget hid) 2)
      ]
    _ -> []

-- | When Revealed: The player with the highest threat level attaches this card to one of
-- his heroes.
instance RunMessage CaughtInAWeb where
  runMessage msg t@(CaughtInAWeb attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      order <- getPlayerOrder
      threats <- for order \p -> (p,) . (.threat) <$> getPlayer p
      let highest = foldr (max . snd) 0 threats
      for_ (headMay [p | (p, n) <- threats, n == highest]) \p -> do
        heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId p)
        unless (null heroes)
          $ push
          $ questionLabelWith (ikey "label.caughtInAWeb.attach") (Just $ toTarget attrs) [] p
            [targetLabel h [AttachTreachery attrs.treacheryId h] | h <- heroes]
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case attrs.placement of
        AttachedTo host -> push $ Ready host
        _ -> pure ()
      pure t
    _ -> CaughtInAWeb <$> runMessage msg attrs
