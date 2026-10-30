module LordOfTheRings.Enemy.Cards.CorsairArsonist (corsairArsonist) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, cdKeywords, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Aflame))

newtype CorsairArsonist = CorsairArsonist EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairArsonist :: EnemyCard CorsairArsonist
corsairArsonist = enemy CorsairArsonist Cards.corsairArsonist

-- | When Revealed: Either deal 2 damage to an Aflame location in play, or search the
-- encounter deck and discard pile for an Aflame location with the highest Aflame value,
-- and add it to the staging area. The first player chooses.
instance RunMessage CorsairArsonist where
  runMessage msg e@(CorsairArsonist attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let aflame c = maximumMay [n | Aflame n <- toList (cdKeywords $ toCardDef c)]
          burning = [(c, n) | c <- scenario.encounterDeck <> scenario.encounterDiscard, cdCardType (toCardDef c) == LocationType, Just n <- [aflame c]]
          highest = maximumMay (map snd burning)
      inPlay <- getAflameLocations
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "corsairArsonist.choose"
        causedBy attrs
        when (notNull inPlay) $ labeled "corsairArsonist.damage" $ dealDamageToAnAflameLocation attrs lead 2
        for_ highest \h ->
          labeled "corsairArsonist.search"
            $ searchEncounterDeckAndDiscard lead attrs
            $ CardMatchAny [cardIs (toCardDef c) | (c, n) <- burning, n == h]
      pure e
    _ -> CorsairArsonist <$> runMessage msg attrs
