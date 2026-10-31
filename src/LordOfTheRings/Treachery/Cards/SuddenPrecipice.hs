module LordOfTheRings.Treachery.Cards.SuddenPrecipice (suddenPrecipice) where

import LordOfTheRings.Location.Grid (Direction (RightOf), toThe)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenPrecipice = SuddenPrecipice TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenPrecipice :: TreacheryCard SuddenPrecipice
suddenPrecipice = treachery SuddenPrecipice Cards.suddenPrecipice

-- | Counts as a Condition attachment with the text: "Forced: After you travel from attached
-- location to the right, deal 2 damage to each exhausted character."
instance HasAbilities SuddenPrecipice where
  getAbilities (SuddenPrecipice a) = [mkAbility a 1 $ forced $ AnyTraveledWindow After | AttachedTo (LocationTarget _) <- [a.placement]]

-- | When Revealed: Attach to the active location.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage SuddenPrecipice where
  runMessage msg t@(SuddenPrecipice attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< select IsActiveLocation
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [lid | AttachedTo (LocationTarget lid) <- [attrs.placement]] \host -> do
        from <- (.position) <$> getLocation host
        for_ [lid | Window _ (Traveled lid) <- ws] \dest -> do
          there <- (.position) <$> getLocation dest
          when (isJust from && there == fmap (toThe RightOf) from) do
            exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
            for_ exhausted \c -> dealDamage attrs c 2
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> SuddenPrecipice <$> runMessage msg attrs
