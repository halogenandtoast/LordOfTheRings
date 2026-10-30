module LordOfTheRings.Treachery.Cards.PutToTheTorch (putToTheTorch) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PutToTheTorch = PutToTheTorch TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

putToTheTorch :: TreacheryCard PutToTheTorch
putToTheTorch = treachery PutToTheTorch Cards.putToTheTorch

-- | When Revealed: Each player must either deal 3 damage to a character he controls, or to
-- an Aflame location in play.
-- Shadow: If the active location has the Aflame keyword, either deal it 3 damage or deal
-- the defending character 1 damage.
instance RunMessage PutToTheTorch where
  runMessage msg t@(PutToTheTorch attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      burning <- map fst <$> getAflameLocations
      eachPlayer \p -> do
        characters <- charactersControlledBy p
        chooseOneM p do
          questionLabeled "putToTheTorch.choose"
          causedBy attrs
          targets characters \c -> dealDamage attrs c 3
          targets burning \l -> dealDamage attrs l 3
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      active <- select IsActiveLocation
      burning <- filterM (fmap (> 0) . getAflame) active
      unless (null burning) do
        chooseOrRunOneM pid do
          questionLabeled "putToTheTorch.shadow"
          causedBy attrs
          targets burning \l -> dealDamage attrs l 3
          targets defenders \d -> dealDamage attrs d 1
      pure t
    _ -> PutToTheTorch <$> runMessage msg attrs
