module LordOfTheRings.Treachery.Cards.Infiltration (infiltration) where

import LordOfTheRings.Trait (Trait (Raider))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Infiltration = Infiltration TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

infiltration :: TreacheryCard Infiltration
infiltration = treachery Infiltration Cards.infiltration

{- | When Revealed: Remove 1 resource from each hero in play. Distribute those resources as
evenly as possible among Raider enemies in play or set aside.
Shadow: Damage from this attack is dealt to an Aflame location in play instead of the
defending character.
-}
instance RunMessage Infiltration where
  runMessage msg t@(Infiltration attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      heroes <- select HeroWithResources
      for_ heroes \h -> removeTokens attrs h Resource 1
      raiders <- select (EnemyWithTrait Raider)
      let n = length heroes
          m = length raiders
      for_ (zip [0 ..] raiders) \(i, e) -> do
        let share = n `div` m + (if i < n `mod` m then 1 else 0)
        when (share > 0) $ placeResources attrs e share
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      burning <- map fst <$> getAflameLocations
      chooseOrRunOneM pid do
        questionLabeled "infiltration.shadow"
        causedBy attrs
        targets burning \l -> for_ defenders \d -> attackModifier (CardIdSource cid) d (DamageRedirectedTo $ toTarget l)
      pure t
    _ -> Infiltration <$> runMessage msg attrs
