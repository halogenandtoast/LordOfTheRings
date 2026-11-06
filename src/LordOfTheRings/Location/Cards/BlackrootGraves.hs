module LordOfTheRings.Location.Cards.BlackrootGraves (blackrootGraves) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Trait (Trait (Undead))

newtype BlackrootGraves = BlackrootGraves LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackrootGraves :: LocationCard BlackrootGraves
blackrootGraves = location BlackrootGraves Cards.blackrootGraves

instance HasAbilities BlackrootGraves where
  getAbilities (BlackrootGraves a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage BlackrootGraves where
  runMessage msg l@(BlackrootGraves a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      pile <- getEncounterDiscard
      for_ (find (cardMatches $ CardWithType EnemyType <> CardWithTrait Undead) pile) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged lead card
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \target -> do
        willpower <- getWillpower target
        when (willpower == 0) $ dealDamage (CardIdSource cid) target 2
      pure l
    _ -> BlackrootGraves <$> runMessage msg a
