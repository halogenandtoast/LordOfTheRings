module LordOfTheRings.Treachery.Cards.Infiltration (infiltration) where

import LordOfTheRings.Card (CardType (EnemyType))
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
      inPlay <- map toTarget <$> select (EnemyWithTrait Raider)
      -- a set-aside Raider keeps its resources for when it enters play
      aside <- getSetAside (CardWithType EnemyType <> CardWithTrait Raider)
      lead <- getLead
      let raiders = inPlay <> [CardIdTarget c.id | c <- aside]
          n = length heroes
          m = length raiders
          give :: ReverseQueue n => Int -> Target -> n ()
          give k = \case
            CardIdTarget cid -> placeResourcesOutOfPlay cid k
            r -> placeResources attrs r k
          -- the first player picks which take the odd ones, 1 each
          extra :: ReverseQueue n => Int -> [Target] -> n ()
          extra k left = when (k > 0) $ chooseOrRunOneM lead do
            questionLabeled "infiltration.extra"
            causedBy attrs
            targets left \r -> give 1 r >> extra (k - 1) (filter (/= r) left)
      when (m > 0 && n > 0) do
        when (n `div` m > 0) $ for_ raiders (give (n `div` m))
        extra (n `mod` m) raiders
      pure t
    ResolveShadowEffect cid _ pid defenders | cid == attrs.cardId -> do
      burning <- map fst <$> getAflameLocations
      chooseOrRunOneM pid do
        questionLabeled "infiltration.shadow"
        causedBy attrs
        targets burning \l -> for_ defenders \d -> attackModifier (CardIdSource cid) d (DamageRedirectedTo $ toTarget l)
      pure t
    _ -> Infiltration <$> runMessage msg attrs
