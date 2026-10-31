module LordOfTheRings.Enemy.Cards.LostSoulOfLorien (lostSoulOfLorien) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Helpers.Character (getWillpower)

newtype LostSoulOfLorien = LostSoulOfLorien EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostSoulOfLorien :: EnemyCard LostSoulOfLorien
lostSoulOfLorien = enemy LostSoulOfLorien Cards.lostSoulOfLorien

-- | Forced: After Lost Soul of Lórien is destroyed, attach it to a hero with 1 or more
-- willpower, if able. (Counts as a condition attachment with the text: "Limit 1 per hero.
-- Attached hero's willpower is reduced to 0.")
-- ponytail: the card goes to the discard pile; its condition lasts on the hero as an
-- effect from the card's code that is never spent
instance RunMessage LostSoulOfLorien where
  runMessage msg (LostSoulOfLorien attrs) = case msg of
    Destroy _ (isTarget attrs -> True) -> do
      let marker = CardCodeSource (toCardCode attrs)
      taken <- getsGame (map effectTarget . filter ((== marker) . effectSource) . gameEffects)
      heroes <- filter ((`notElem` taken) . toTarget) <$> select AnyHero
      willing <- filterM (fmap (>= 1) . getWillpower . toTarget) heroes
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "lostSoulOfLorien.attach"
        causedBy attrs
        targets willing \h -> createEffect marker h [WillpowerModifier (-99)] UntilSpent
      LostSoulOfLorien <$> runMessage msg attrs
    _ -> LostSoulOfLorien <$> runMessage msg attrs
